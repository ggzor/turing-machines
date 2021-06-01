{-# LANGUAGE StrictData #-}

module TuringMachines.Core where

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import GHC.Generics (Generic)

data Bit = B0 | B1 deriving (Eq, Generic)

data Dir = L | R deriving (Eq, Generic)

data Action = SetTo Bit | MoveTo Dir deriving (Eq, Generic)

data Transition a = Transition Action a | Halt deriving (Eq, Generic)

data Spec a = Spec (Transition a) (Transition a) deriving (Eq, Generic)

type Program a = M.Map a (Spec a)

type Index = Int

type Tape = IM.IntMap Bit

data State a = State a Index Tape deriving (Eq, Generic)

mapProgram :: Ord b => (a -> b) -> Program a -> Program b
mapProgram f = M.mapKeys f . M.map (mapSpec f)

mapSpec :: (a -> b) -> Spec a -> Spec b
mapSpec f (Spec if0 if1) = Spec (mapTrans f if0) (mapTrans f if1)

mapTrans :: (a -> b) -> Transition a -> Transition b
mapTrans f (Transition a i) = Transition a (f i)
mapTrans _ Halt = Halt
