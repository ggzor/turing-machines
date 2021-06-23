{-# LANGUAGE TemplateHaskell #-}

module Commands.Eval.Speculative where

import Control.Lens (makeLenses, (^.))
import qualified Data.IntMap.Strict as IntMap
import TuringMachines.Core
import TuringMachines.Eval (eval)

data SpeculativeData = SpeculativeData
  { _totalSteps :: !Int
  , _minimumIdx :: !Int
  , _maximumIdx :: !Int
  }

makeLenses ''SpeculativeData

speculativeEval :: Program Integer -> State Integer -> Maybe Int -> SpeculativeData
speculativeEval program state@(State _ idx tape) limitSteps =
  go
    state
    SpeculativeData
      { _totalSteps = 0
      , _minimumIdx = min idx (maybe idx fst (IntMap.lookupMin tape))
      , _maximumIdx = max idx (maybe idx fst (IntMap.lookupMin tape))
      }
  where
    go s prevData =
      if maybe False (prevData ^. totalSteps >=) limitSteps
        then prevData
        else case eval program s of
          Nothing -> prevData
          Just newState@(State _ newIdx _) ->
            go
              newState
              SpeculativeData
                { _totalSteps = prevData ^. totalSteps + 1
                , _minimumIdx = min (prevData ^. minimumIdx) newIdx
                , _maximumIdx = max (prevData ^. maximumIdx) newIdx
                }
