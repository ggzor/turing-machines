module TuringMachines.Normalize (normalize, NormalizedProgram, getProgram) where

import qualified Data.Map.Strict as M
import TuringMachines.Core

newtype NormalizedProgram a = Normalized {getProgram :: Program a}

normalize :: (Enum a, Ord a) => Program a -> NormalizedProgram a
normalize program =
  let (maxState, usesNew) =
        let (maxSt, sp) = M.findMax program
         in if sp == Spec Halt Halt then (maxSt, False) else (succ maxSt, True)
      setGoMax bit = Transition (SetTo bit) maxState
      s0gm = setGoMax B0
      s1gm = setGoMax B1
      normTrans Halt = Halt
      normTrans (Transition a s) = Transition a (min maxState s)
      normSpec (Spec t0 t1) =
        let n0 = normTrans t0
            n1 = normTrans t1
         in case (n0, n1) of
              (Halt, Halt) -> Spec s0gm s1gm
              (tf0, Halt) -> Spec tf0 s1gm
              (Halt, tf1) -> Spec s0gm tf1
              (tf0, tf1) -> Spec tf0 tf1
   in if M.null program
        then Normalized program
        else Normalized ((if usesNew then id else M.deleteMax) $ M.map normSpec program)
