module Abacus.Compiler where

import Abacus.Core
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import TuringMachines.Core
import qualified TuringMachines.Core as TM
import qualified TuringMachines.PPrint as PP
import TuringMachines.Show ()
import Utils.QString

programAsStr :: Program Integer -> String
programAsStr = T.unpack . PP.pprint . mapProgram QString

compile :: FlowChart Integer Integer -> Maybe (TM.Program Integer)
compile fc =
  let action = do
        tagIndex <- M.fromList <$> fc `forM` \(Seq tag _) -> (tag,) <$> fresh
        sequents <-
          M.fromList <$> zip fc (tail fc) `forM` \(Seq t1 _, Seq t2 _) -> do
            (t1,) <$> lift (M.lookup t2 tagIndex)
        M.fromList . concat <$> fc `forM` \(Seq tag nodes) -> do
          qTag <- lift $ M.lookup tag tagIndex
          qInitial <- fresh
          result <-
            concat <$> nodes `forM` \case
              Increase i -> compileIncrease i
              Decrease i tag -> do
                target <- lift $ M.lookup tag tagIndex
                compileDecrease i target
              GoTo tag -> do
                target <- lift $ M.lookup tag tagIndex
                q <- lastQ
                -- Generate fresh state because current is being used
                _ <- fresh
                pure $ compileGoTo q target
          sequentLink <- case M.lookup tag sequents of
            Nothing -> pure []
            Just qNext -> do
              qLast <- lastQ
              pure $ compileGoTo qLast qNext
          pure $ compileGoTo qTag qInitial ++ sequentLink ++ result
   in action `evalStateT` QGen 0

compileDecrease :: (MonadState QGen m, MonadFail m) => Integer -> Q -> m NodeSeq
compileDecrease i qIfZero =
  let checkIfZero = do
        q1 <- lastQ
        [q2, qMoveToLeftmost] <- replicateM 2 fresh

        moveToLeftmostActions <- moveToLeftmost
        qWhenZero <- lastQ

        qIfNotZero <- fresh
        pure . concat $
          [
            [ (q1, Spec (Transition (SetTo B1) q1) (Transition (MoveTo R) q2))
            , (q2, Spec (Transition (MoveTo L) qMoveToLeftmost) (Transition (MoveTo R) qIfNotZero))
            ]
          , moveToLeftmostActions
          ,
            [
              ( qWhenZero
              , Spec (Transition (SetTo B0) qIfZero) (Transition (SetTo B1) qIfZero)
              )
            ]
          ]

      eraseOneAndDisplaceLeft = do
        q1 <- lastQ
        [q2, q3, q4, q5] <- replicateM 4 fresh
        pure
          [ (q1, Spec (Transition (MoveTo L) q2) (Transition (MoveTo R) q1))
          , (q2, Spec (Transition (MoveTo R) q3) (Transition (SetTo B0) q2))
          , (q3, Spec (Transition (SetTo B1) q3) (Transition (MoveTo R) q4))
          , (q4, Spec (Transition (MoveTo L) q5) (Transition (MoveTo R) q1))
          ]
   in fmap concat . sequence $
        [ moveToRight i
        , checkIfZero
        , eraseOneAndDisplaceLeft
        , moveToLeftmost
        ]

compileIncrease :: (MonadState QGen m, MonadFail m) => Integer -> m NodeSeq
compileIncrease i = do
  let increaseAndDisplaceRight = do
        q1 <- lastQ
        [q2, q3, q4, q5, q6, q7] <- replicateM 6 fresh
        pure
          [ (q1, Spec (Transition (SetTo B1) q1) (Transition (MoveTo R) q2))
          , (q2, Spec (Transition (SetTo B1) q3) (Transition (MoveTo R) q2))
          , (q3, Spec Halt (Transition (MoveTo R) q4))
          , (q4, Spec (Transition (MoveTo L) q7) (Transition (SetTo B0) q5))
          , (q5, Spec (Transition (MoveTo R) q6) Halt)
          , (q6, Spec (Transition (SetTo B1) q3) (Transition (MoveTo R) q6))
          ]
   in fmap concat . sequence $
        [ moveToRight i
        , increaseAndDisplaceRight
        , moveToLeftmost
        ]

compileGoTo :: Q -> Q -> NodeSeq
compileGoTo src dest =
  [(src, Spec (Transition (SetTo B0) dest) (Transition (SetTo B1) dest))]

type Q = Integer
type SpecLine = (Q, Spec Q)
type NodeSeq = [SpecLine]

newtype QGen = QGen {unQGen :: Integer}

fresh :: (MonadState QGen m) => m Integer
fresh = modify' (QGen . (+ 1) . unQGen) >> (unQGen <$> get)

lastQ :: (MonadState QGen m) => m Integer
lastQ = unQGen <$> get

-- Moves the machine to the first cell, going from right to left
-- starting from the rightmost 1 of any cell.
moveToLeftmost :: (MonadState QGen m, MonadFail m) => m NodeSeq
moveToLeftmost = do
  q1 <- lastQ
  [q2, q3, qfinal] <- replicateM 3 fresh
  pure
    [ (q1, Spec (Transition (MoveTo L) q2) (Transition (MoveTo L) q1))
    , (q2, Spec (Transition (MoveTo R) q3) (Transition (MoveTo L) q1))
    , (q3, Spec (Transition (MoveTo R) q3) (Transition (SetTo B1) qfinal))
    ]

-- Moves the machine n cells to the right
-- starting from the leftmost 1 or 0 of any cell;
-- substituting B-representation for 1-representation of empty cells
moveToRight :: (MonadState QGen m, MonadFail m) => Integer -> m NodeSeq
moveToRight n =
  concat <$> [1 .. (n - 1)] `forM` const do
    q1 <- lastQ
    [q2, qfinal] <- replicateM 2 fresh
    pure
      [ (q1, Spec (Transition (SetTo B1) q1) (Transition (MoveTo R) q2))
      , (q2, Spec (Transition (MoveTo R) qfinal) (Transition (MoveTo R) q2))
      ]
