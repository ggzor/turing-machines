module TuringMachines.Quads where

import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import TuringMachines.Core

type Quad a = (a, Bit, Action, a)

asQuads :: Program a -> [Quad a]
asQuads program = flattenSpec =<< M.toAscList program

flattenSpec :: (a, Spec a) -> [Quad a]
flattenSpec (initial, Spec t1 t2) =
  catMaybes $ fmap (append initial) . asSemiquad <$> [(B0, t1), (B1, t2)]

asSemiquad :: (Bit, Transition a) -> Maybe (Bit, Action, a)
asSemiquad (_, Halt) = Nothing
asSemiquad (bit, Transition action final) = Just (bit, action, final)

append :: a -> (b, c, d) -> (a, b, c, d)
append a (b, c, d) = (a, b, c, d)
