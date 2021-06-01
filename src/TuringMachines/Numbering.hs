{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TuringMachines.Numbering where

import Control.Monad (guard)
import Data.Bifunctor (bimap)
import qualified Data.List as L
import Data.List.Utils (map2)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Math.Primes (countPrimes, primes)
import TuringMachines.Core
import TuringMachines.Normalize

actionAsInt :: Action -> Integer
actionAsInt (SetTo B0) = 1
actionAsInt (SetTo B1) = 2
actionAsInt (MoveTo L) = 3
actionAsInt (MoveTo R) = 4

numberAsAction :: Integer -> Maybe Action
numberAsAction 1 = Just (SetTo B0)
numberAsAction 2 = Just (SetTo B1)
numberAsAction 3 = Just (MoveTo L)
numberAsAction 4 = Just (MoveTo R)
numberAsAction _ = Nothing

allStates :: Ord a => Program a -> S.Set a
allStates program =
  let getTransStates Halt = S.empty
      getTransStates (Transition _ a) = S.singleton a
      getStates s (Spec t1 t2) = S.unions [s, getTransStates t1, getTransStates t2]
   in M.foldl' getStates S.empty program

programAsSequence :: Ord a => NormalizedProgram a -> [Integer]
programAsSequence (getProgram -> program) =
  let states =
        M.fromDistinctAscList $
          zip (S.toAscList $ M.keysSet program `S.union` allStates program) [1 ..]
      extend (_, Spec t1 t2) =
        let ts = [t1, t2]
            [x, y] = map (\(Transition act _) -> actionAsInt act) ts
            [Just dest1, Just dest2] = map (\(Transition _ a) -> M.lookup a states) ts
         in [x, dest1, y, dest2]
   in concatMap extend $ M.toAscList program

programAsNumber :: Ord a => NormalizedProgram a -> Integer
programAsNumber program = L.foldl' (*) 1 $ zipWith (^) primes (programAsSequence program)

numberAsProgram :: Integer -> Maybe (Program Integer)
numberAsProgram n =
  let (rawActions, states) =
        bimap (map fst) (map fst) . L.partition (even @Integer . snd) $
          zip (map snd (countPrimes n)) [0 ..]
      mActions = traverse numberAsAction rawActions
   in do
        actions <- mActions
        guard (length actions == length states)
        guard (even $ length actions)
        guard (all (> 0) states)
        let transitions = zipWith Transition actions states
        pure . M.fromDistinctAscList . zip [1 ..] $ map2 Spec transitions
