{-# LANGUAGE TemplateHaskell #-}

module Commands.Eval.Runner where

import Commands.Eval.Parser (EvalOptions (EvalOptions))
import qualified Commands.Eval.Parser as P
import Commands.Eval.Speculative

import Control.Lens (makeLenses, preview, use, view, (%=), (+=), (^.), _Just)
import Control.Monad (foldM, void)
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.String.Interpolate
import Data.Text (justifyRight)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Utils (tshow)
import Graphviz
import RIO (MonadReader, ReaderT (runReaderT))
import RIO.FilePath ((</>))
import RIO.State (MonadState, evalStateT, get)
import SVG hiding (speculativeData, _speculativeData)
import System.Console.ANSI
import System.Directory (
  copyFile,
  createDirectoryIfMissing,
  doesDirectoryExist,
  listDirectory,
  removeDirectoryRecursive,
 )
import System.IO (hFlush, stdout)
import TuringMachines.Core
import TuringMachines.Eval (eval, readTape)
import UnliftIO (MonadIO)
import Utils

data EvalOutputMode
  = ConsoleOut
  | ImageOut FilePath
  | DirectoryOut FilePath
  deriving
    (Ord, Eq)

data EvalConfiguration = EvalConfiguration
  { _options :: !EvalOptions
  , _renderOptions :: !RenderOptions
  , _computedRenderOptions :: !(Maybe ComputedRenderOptions)
  , _outputModes :: ![EvalOutputMode]
  , _speculativeData :: !(Maybe SpeculativeData)
  }

makeLenses ''EvalConfiguration

processEval :: EvalOptions -> Program Integer -> Tape -> IO ()
processEval
  opts@EvalOptions
    { P.doNotEvalSpeculatively
    , P.limitSteps
    , P.stepOutput
    , P.outputDirectory
    , P.renderOptions
    }
  program
  tape = do
    let _outputModes =
          case (stepOutput, outputDirectory) of
            (Nothing, Nothing) -> [ConsoleOut]
            (Nothing, Just outDir) -> [DirectoryOut outDir]
            (imOut, outDir) ->
              catMaybes
                [ DirectoryOut <$> outDir
                , ImageOut <$> imOut
                , Just ConsoleOut
                ]

    _computedRenderOptions <- case _outputModes of
      [ConsoleOut] -> pure Nothing
      _ -> fmap (computedRenderOptionsFor renderOptions) <$> getGraphRenderData program

    let initialQ = 1
    let initialIdx = 0
    let initialState = State initialQ initialIdx tape

    let speculativeData = speculativeEval program initialState limitSteps
    let adjustedTape =
          let minIdx = speculativeData ^. minimumIdx
              maxIdx = speculativeData ^. maximumIdx
           in tape
                & IntMap.insert minIdx (fromMaybe B0 (IntMap.lookup minIdx tape))
                & IntMap.insert maxIdx (fromMaybe B0 (IntMap.lookup maxIdx tape))
    let adjustedState = State 1 0 adjustedTape

    processEval' program (if doNotEvalSpeculatively then initialState else adjustedState)
      & ( `runReaderT`
            EvalConfiguration
              { _options = opts
              , _renderOptions = renderOptions
              , _computedRenderOptions
              , _outputModes
              , _speculativeData =
                  if doNotEvalSpeculatively
                    then Nothing
                    else Just speculativeData
              }
        )
      & (`evalStateT` StateRenderOptions{_steps = 0, _pivot = initialIdx})

processEval' ::
  ( MonadIO m
  , MonadState StateRenderOptions m
  , MonadReader EvalConfiguration m
  ) =>
  Program Integer ->
  State Integer ->
  m ()
processEval' program state@(State _ idx _) = do
  EvalOptions{P.lineByLine, P.limitSteps} <- view options

  generatedImage <-
    view computedRenderOptions >>= \case
      Just computed -> do
        pivot %= relocatePivot (computed ^. cellCount) idx
        renderOptions <- view renderOptions
        stateRenderOptions <- get
        speculativeData <- view speculativeData
        let settings =
              RenderSettings
                renderOptions
                computed
                stateRenderOptions
                speculativeData
        liftIO $ printImage settings program state
      _ -> pure Nothing

  currentSteps <- use steps

  view outputModes >>= mapM_ \case
    ConsoleOut -> liftIO $ do
      pprintState program state
      if lineByLine
        then hFlush stdout >> void getLine
        else putStrLn ""
    ImageOut targetImage ->
      case generatedImage of
        Nothing -> pure ()
        Just path ->
          liftIO $ copyFile path targetImage
    DirectoryOut dir -> case generatedImage of
      Nothing -> pure ()
      Just path -> do
        totalSteps <- preview $ speculativeData . _Just . totalSteps
        liftIO do
          let totalString = maybe "" (("/" ++) . show) totalSteps
              digits = maybe 3 (length . show) totalSteps
              msgString = [i|Generando imagenes #{currentSteps}#{totalString}|]
          putStrLn msgString

          askProceed <-
            foldM
              (\acc next -> if acc then next else pure False)
              True
              [ pure $ currentSteps == 0
              , doesDirectoryExist dir
              , not . null <$> listDirectory dir
              ]

          if askProceed
            then do
              putStrLn [i|El directorio '#{dir}' existe y será borrado.|]
              putStr "Proceder? [y/n] " >> hFlush stdout

              answer <- getLine
              if answer == "y"
                then removeDirectoryRecursive dir
                else exitError "Cancelado"
            else pure ()

          let stepStr = justifyRight digits '0' . tshow $ currentSteps
              targetFile = dir </> [i|step-#{stepStr}.png|]
          createDirectoryIfMissing True dir
          copyFile path targetFile

  if maybe False (currentSteps >=) limitSteps
    then pure ()
    else do
      let newState = eval program state
      steps += 1
      case newState of
        Nothing -> pure ()
        Just st ->
          processEval'
            program
            st

pprintState :: Program Integer -> State Integer -> IO ()
pprintState program (State q idx tape) = do
  let maxStateLength = length . show $ maybe 0 fst (M.lookupMax program)
      minIdx = maybe idx fst (IntMap.lookupMin tape)
      maxIdx = maybe idx fst (IntMap.lookupMax tape)

  let stateStr = T.justifyLeft (maxStateLength + 3) ' ' [i|<q#{q}>|]
  TIO.putStr [i|#{stateStr} #{tapeInterval minIdx idx}|]

  setSGR [SetUnderlining SingleUnderline, SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
  putChar . bitToStr $ readTape idx tape
  setSGR [Reset]

  putStr $ tapeInterval (idx + 1) (maxIdx + 1)
  where
    tapeInterval :: Index -> Index -> String
    tapeInterval start end = bitToStr . flip readTape tape <$> [start .. end - 1]

relocatePivot :: Int -> Index -> Index -> Index
relocatePivot n newIdx pivot =
  let (minIdx, maxIdx) = pivotBounds n pivot
   in if
          | newIdx < minIdx -> pivot - (minIdx - newIdx)
          | maxIdx < newIdx -> pivot + (newIdx - maxIdx)
          | otherwise -> pivot

bitToStr :: Bit -> Char
bitToStr b = case b of
  B0 -> '0'
  B1 -> '1'
