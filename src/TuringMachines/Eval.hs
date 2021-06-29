{-# LANGUAGE Strict #-}

module TuringMachines.Eval where

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import TuringMachines.Core

readTape :: Index -> Tape -> Bit
readTape idx tape = fromMaybe B0 $ IM.lookup idx tape

transitionForState :: Ord a => Program a -> State a -> Transition a
transitionForState program (State q idx tape) =
  let (Spec if0 if1) = fromMaybe (Spec Halt Halt) (M.lookup q program)
   in case readTape idx tape of
        B0 -> if0
        B1 -> if1

eval :: Ord a => Program a -> State a -> Maybe (State a)
eval program state@(State _ idx tape) =
  case transitionForState program state of
    Halt -> Nothing
    Transition action nextQ ->
      let (newIdx, newTape) = case action of
            SetTo bit -> (idx, IM.insert idx bit tape)
            MoveTo L -> (idx - 1, tape)
            MoveTo R -> (idx + 1, tape)
       in Just $ State nextQ newIdx newTape

evalFull :: Ord a => Program a -> State a -> State a
evalFull program = go
  where
    go state =
      maybe state go (eval program state)
