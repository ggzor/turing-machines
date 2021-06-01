module Main where

import Data.Function ((&))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Fmt
import Math.Primes (primes)
import Options.Applicative hiding (action)
import RIO (readFileUtf8)
import RIO.Directory (doesFileExist)
import RIO.Text (Text, pack, unpack)
import System.Console.ANSI
import System.Exit (exitFailure)
import TuringMachines.Core
import TuringMachines.Eval (eval, readTape)
import TuringMachines.Normalize (getProgram, normalize)
import TuringMachines.Numbering
import qualified TuringMachines.PPrint as PP
import qualified TuringMachines.Parser as TP

data Options
  = Numbered Integer
  | Info FilePath
  | Eval FilePath Tape

options :: Parser Options
options =
  subparser
    ( command "numbered" (info numberedOptions (progDesc "Muestra el programa correspondiente al numero dado"))
        <> command "info" (info infoOptions (progDesc "Muestra información relevante del programa dado"))
        <> command "eval" (info evalOptions (progDesc "Evalua el programa con la entrada dada"))
    )

numberedOptions :: Parser Options
numberedOptions =
  Numbered <$> argument auto (metavar "N" <> help "El numero del programa a mostrar")

infoOptions :: Parser Options
infoOptions = Info <$> fileArgument

evalOptions :: Parser Options
evalOptions =
  Eval <$> fileArgument
    <*> argument
      (maybeReader (TP.parseTape . pack))
      (metavar "INPUT" <> help "La entrada binaria para el programa")

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
doWork (Info path) = withExistentFile path processInfo
doWork (Eval path input) = withExistentFile path $ \pathText -> do
  case TP.parse pathText of
    Just program ->
      let (minIdx, maxIdx) = dangerouslyDetermineBounds program (State 1 0 input)
          initialTape =
            input
              & IntMap.insert minIdx (fromMaybe B0 (IntMap.lookup minIdx input))
              & IntMap.insert maxIdx (fromMaybe B0 (IntMap.lookup maxIdx input))
       in processEval program (State 1 0 initialTape)
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

processInfo :: Text -> IO ()
processInfo t =
  case TP.parse t of
    Nothing -> putStrLn "El programa no es valido"
    Just p -> do
      putStrLn "Original: "
      putStrLn $ programAsStr p
      printNormalizedIfDifferent p
      let normalized = normalize p
      fmtLn $ "Numero: " +| programAsNumber normalized |+ ""
      putStrLn "Sequencia de potencias de primos: "
      putStrLn $
        unwords . zipWith (\prime n -> fmt $ prime |+ "^" +| n |+ "") primes $
          programAsSequence normalized

-- Print the program as states prefixed with 'q'
newtype QString = QString Integer deriving (Eq, Ord)

instance Show QString where
  show (QString i) = "q" ++ show i

programAsStr :: Program Integer -> String
programAsStr = unpack . PP.pprint . mapProgram QString

processEval :: Program Integer -> State Integer -> IO ()
processEval program state = do
  pprintState program state
  let newState = eval program state
  case newState of
    Nothing -> pure ()
    Just st -> processEval program st

pprintState :: Program Integer -> State Integer -> IO ()
pprintState program (State q idx tape) = do
  let maxStateLength = length . show $ maybe 0 fst (M.lookupMax program)
      minIdx = maybe idx fst (IntMap.lookupMin tape)
      maxIdx = maybe idx fst (IntMap.lookupMax tape)
  fmt $ padRightF (maxStateLength + 3) ' ' (("<q" +| q |+ ">") :: Text) |+ " " +| tapeInterval minIdx idx |+ ""
  setSGR [SetSwapForegroundBackground True]
  putChar . bitToStr $ readTape idx tape
  setSGR [Reset]
  putStrLn $ tapeInterval (idx + 1) (maxIdx + 1)
  where
    tapeInterval :: Index -> Index -> String
    tapeInterval start end = bitToStr . flip readTape tape <$> [start .. end - 1]

bitToStr :: Bit -> Char
bitToStr b = case b of
  B0 -> '0'
  B1 -> '1'
