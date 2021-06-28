module Abacus.Eval where

import Abacus.Core
import Data.Function ((&))
import Data.IntMap
import qualified Data.IntMap as IntMap
import qualified Data.List as L

data EvalState = EvalState (Maybe (Integer, Integer)) (IntMap Integer) deriving (Show)

evalWithState :: FlowChart Integer Integer -> EvalState -> Maybe EvalState
evalWithState fc (EvalState (Just coords) cells) = do
  (resolvedCoords, node) <- resolve coords fc
  let nextCoords = resolveNext resolvedCoords fc
  pure $ case node of
    Increase i -> EvalState nextCoords $ cells & IntMap.insertWith (+) (fromIntegral i) 1
    Decrease i tag ->
      case IntMap.lookup (fromIntegral i) cells of
        Nothing -> EvalState (Just (tag, 0)) cells
        Just 0 -> EvalState (Just (tag, 0)) cells
        Just x -> do
          EvalState nextCoords $ IntMap.insert (fromIntegral i) (x - 1) cells
    GoTo tag -> EvalState (Just (tag, 0)) cells
evalWithState _ (EvalState Nothing _) = Nothing

resolve :: (Integer, Integer) -> FlowChart Integer Integer -> Maybe ((Integer, Integer), Node Integer Integer)
resolve (tag, idx) fc = do
  tagIdx <- L.findIndex (\(Seq t _) -> t == tag) fc
  let Seq _ nodes = fc !! tagIdx
  if fromIntegral idx >= length nodes
    then
      if tagIdx >= length fc - 1
        then Nothing
        else
          let Seq nextTag _ = fc !! (tagIdx + 1)
           in resolve (nextTag, 0) fc
    else Just ((tag, fromIntegral idx), nodes !! fromIntegral idx)

resolveNext :: (Integer, Integer) -> FlowChart Integer Integer -> Maybe (Integer, Integer)
resolveNext (tag, idx) fc = do
  tagIdx <- L.findIndex (\(Seq t _) -> t == tag) fc
  let Seq _ nodes = fc !! tagIdx
  if fromIntegral idx >= length nodes - 1
    then
      if tagIdx >= length fc - 1
        then Nothing
        else
          let Seq nextTag _ = fc !! (tagIdx + 1)
           in Just (nextTag, 0)
    else Just (tag, idx + 1)

eval :: FlowChart Integer Integer -> IntMap Integer -> IntMap Integer
eval [] cells = cells
eval fc@(Seq initialTag _ : _) initialCells =
  go (EvalState (Just (initialTag, 0)) initialCells)
  where
    go state@(EvalState _ cells) =
      evalWithState fc state & maybe cells go
