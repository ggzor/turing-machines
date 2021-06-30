{-# LANGUAGE ApplicativeDo #-}

module Parser where

import Commands.Abacus.Parser
import Commands.Eval.Parser
import Commands.Info.Parser
import Commands.Numbered.Parser

import Data.Foldable (fold)
import Options.Applicative hiding (action)

data Commands
  = Numbered !NumberedCommandOptions
  | Info !InfoCommandOptions
  | Eval !EvalCommandOptions
  | Abacus !AbacusCommandOptions

options :: Parser Commands
options =
  subparser . fold $
    [ command "numbered" . info (Numbered <$> numberedCommandOptions <**> helper) $
        progDesc "Muestra el programa correspondiente al numero dado"
    , command "info" . info (Info <$> infoOptions <**> helper) $
        progDesc "Muestra información relevante del programa dado"
    , command "eval" . info (Eval <$> evalCommandOptions <**> helper) $
        progDesc "Evalua el programa con la entrada dada"
    , command "abacus" . info (Abacus <$> abacusOptions <**> helper) $
        progDesc "Ejecuta operaciones sobre programas de abaco"
    ]
