{-# OPTIONS_GHC -Wno-orphans #-}

module TuringMachines.Show where

import TuringMachines.Core

instance Show Bit where
  show B0 = "0"
  show B1 = "1"

deriving instance Show Dir

instance Show Action where
  show (SetTo B0) = "0"
  show (SetTo B1) = "1"
  show (MoveTo L) = "L"
  show (MoveTo R) = "R"

instance Show a => Show (Transition a) where
  show (Transition action state) = show action ++ show state
  show Halt = "_"

deriving instance Show a => Show (State a)

deriving instance Show a => Show (Spec a)
