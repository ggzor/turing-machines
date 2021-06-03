module Main where

import Control.Monad (forM_, void)
import Data.Function ((&))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Fmt (Builder, fmt, fmtLn, padRightF, (+|), (|+))
import Math.Primes (primes)
import Options.Applicative hiding (action)
import RIO (readFileUtf8)
import RIO.Directory (doesFileExist)
import RIO.Text (Text, pack, unpack)
import System.Console.ANSI
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)
import TuringMachines.Core
import TuringMachines.Eval (eval, readTape)
import TuringMachines.Graphviz
import TuringMachines.Normalize (getProgram, normalize)
import TuringMachines.Numbering
import qualified TuringMachines.PPrint as PP
import qualified TuringMachines.Parser as TP
import Utils.QString

data EvalOptions = EvalOptions
  { doNotEvalSpeculatively :: !Bool,
    lineByLine :: !Bool
  }

data FormatOption = Original | Normalized | Number | PrimeSeq | Graph
  deriving (Ord, Eq, Show)

data InfoOptions = InfoOptions
  { format :: [FormatOption]
  }

data Options
  = Numbered !Integer
  | Info !FilePath !InfoOptions
  | Eval !FilePath !Tape !EvalOptions

options :: Parser Options
options =
  subparser
    ( command "numbered" (info numberedOptions (progDesc "Muestra el programa correspondiente al numero dado"))
        <> command "info" (info (infoOptions <**> helper) (progDesc "Muestra información relevante del programa dado"))
        <> command "eval" (info (evalOptions <**> helper) (progDesc "Evalua el programa con la entrada dada"))
    )

numberedOptions :: Parser Options
numberedOptions =
  Numbered <$> argument auto (metavar "N" <> help "El numero del programa a mostrar")

infoOptions :: Parser Options
infoOptions =
  Info
    <$> fileArgument
    <*> ( InfoOptions
            <$> option
              (eitherReader parseFormatOptionList)
              ( short 'f'
                  <> long "format"
                  <> metavar "FORMAT"
                  <> value [Original, Normalized, Number, PrimeSeq]
                  <> help
                    ( "Formatos disponibles:"
                        ++ " o = Programa original"
                        ++ ", n = Programa normalizado"
                        ++ ", u = Numero de Godel"
                        ++ ", s = Sequencia de primos"
                        ++ ", g = Grafo"
                    )
              )
        )

parseFormatOptionList :: String -> Either String [FormatOption]
parseFormatOptionList = fmap L.nub . traverse parseFormatOption

parseFormatOption :: Char -> Either String FormatOption
parseFormatOption 'o' = Right Original
parseFormatOption 'n' = Right Normalized
parseFormatOption 'u' = Right Number
parseFormatOption 's' = Right PrimeSeq
parseFormatOption 'g' = Right Graph
parseFormatOption c = Left $ "Formato desconocido: " ++ [c]

evalOptions :: Parser Options
evalOptions =
  Eval <$> fileArgument
    <*> argument
      (maybeReader (TP.parseTape . pack))
      (metavar "INPUT" <> help "La entrada binaria para el programa")
    <*> ( EvalOptions
            <$> switch
              ( short 'n'
                  <> help
                    ( "No evaluar especulativamente: "
                        ++ "Provoca una salida menos agradable pero al menos muestra salida"
                    )
              )
            <*> switch
              ( short 'l'
                  <> help "Evaluar linea por linea"
              )
        )

fileArgument :: Parser FilePath
fileArgument = argument str (metavar "FILE" <> help "El archivo que contiene el programa")

main :: IO ()
main = doWork =<< execParser opts
  where
    opts =
      info
        (options <**> helper)
        ( fullDesc
            <> header "turing-machines - Un interprete de maquinas de Turing"
        )

exitError :: Builder -> IO ()
exitError message = do
  fmtLn message
  exitFailure

doWork :: Options -> IO ()
doWork (Numbered n) = processNumbered n
doWork (Info path opts) = withExistentFile path (processInfo opts)
doWork (Eval path input opts@EvalOptions {doNotEvalSpeculatively}) = withExistentFile path $ \pathText -> do
  case TP.parse pathText of
    Just program ->
      let (minIdx, maxIdx) = dangerouslyDetermineBounds program (State 1 0 input)
          adjustedTape =
            input
              & IntMap.insert minIdx (fromMaybe B0 (IntMap.lookup minIdx input))
              & IntMap.insert maxIdx (fromMaybe B0 (IntMap.lookup maxIdx input))
          initialState =
            State 1 0 $
              if doNotEvalSpeculatively
                then input
                else adjustedTape
       in processEval opts program initialState
    Nothing -> exitError "El programa no es valido"

dangerouslyDetermineBounds :: forall a. (Ord a) => Program a -> State a -> (Index, Index)
dangerouslyDetermineBounds prog st@(State _ idx tape) =
  go
    prog
    st
    ( min idx (maybe idx fst (IntMap.lookupMin tape)),
      max idx (maybe idx fst (IntMap.lookupMax tape))
    )
  where
    go :: Program a -> State a -> (Index, Index) -> (Index, Index)
    go p s r@(minIdx, maxIdx) =
      case eval p s of
        Just nextState@(State _ nidx _) -> go p nextState (min minIdx nidx, max maxIdx nidx)
        Nothing -> r

withExistentFile :: FilePath -> (Text -> IO ()) -> IO ()
withExistentFile path action = do
  exists <- doesFileExist path
  if exists
    then readFileUtf8 path >>= action
    else exitError $ "El archivo '" +| path |+ "' no existe"

processNumbered :: Integer -> IO ()
processNumbered n =
  case numberAsProgram n of
    Nothing -> do
      exitError "El numero no representa un programa valido"
    Just p -> do
      putStrLn (programAsStr p)
      printNormalizedIfDifferent p

printNormalizedIfDifferent :: Program Integer -> IO ()
printNormalizedIfDifferent p
  | norm /= p = putStrLn "Normalizado:" >> putStrLn (programAsStr norm)
  | otherwise = pure ()
  where
    norm = getProgram . normalize $ p

processInfo :: InfoOptions -> Text -> IO ()
processInfo InfoOptions {format} t =
  case TP.parse t of
    Nothing -> putStrLn "El programa no es valido"
    Just p -> do
      let label = if length format > 1 then putStrLn else void . pure
      let normalized = normalize p

      if format == [Normalized, Graph]
        then fmtLn $ generateGraph (getProgram normalized) |+ ""
        else do
          forM_ format $ \case
            Original -> do
              label "Original: "
              putStrLn $ programAsStr p
            Normalized -> do
              label "Normalizado: "
              putStrLn $ programAsStr (getProgram normalized)
            Number -> do
              label "Numero de Godel:"
              print $ programAsNumber normalized
            PrimeSeq -> do
              label "Secuencia de potencias de primos: "
              putStrLn $
                unwords . zipWith (\prime n -> fmt $ prime |+ "^" +| n |+ "") primes $
                  programAsSequence normalized
            Graph -> do
              label "Grafo: "
              fmtLn $ generateGraph p |+ ""

programAsStr :: Program Integer -> String
programAsStr = unpack . PP.pprint . mapProgram QString

processEval :: EvalOptions -> Program Integer -> State Integer -> IO ()
processEval opts@EvalOptions {lineByLine} program state = do
  pprintState program state
  if lineByLine
    then hFlush stdout >> void getLine
    else putStrLn ""
  let newState = eval program state
  case newState of
    Nothing -> pure ()
    Just st -> processEval opts program st

pprintState :: Program Integer -> State Integer -> IO ()
pprintState program (State q idx tape) = do
  let maxStateLength = length . show $ maybe 0 fst (M.lookupMax program)
      minIdx = maybe idx fst (IntMap.lookupMin tape)
      maxIdx = maybe idx fst (IntMap.lookupMax tape)
  fmt $ padRightF (maxStateLength + 3) ' ' (("<q" +| q |+ ">") :: Text) |+ " " +| tapeInterval minIdx idx |+ ""
  setSGR [SetUnderlining SingleUnderline, SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
  putChar . bitToStr $ readTape idx tape
  setSGR [Reset]
  putStr $ tapeInterval (idx + 1) (maxIdx + 1)
  where
    tapeInterval :: Index -> Index -> String
    tapeInterval start end = bitToStr . flip readTape tape <$> [start .. end - 1]

bitToStr :: Bit -> Char
bitToStr b = case b of
  B0 -> '0'
  B1 -> '1'
