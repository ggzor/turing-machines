module Abacus.Compiler where

import Abacus.Core
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import GHC.IO (unsafePerformIO)
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
        tagIndex <- reserveTags fc
        M.fromList . concat <$> fc `forM` \(Seq tag nodes) -> do
          port <- lift $ M.lookup tag tagIndex
          initial <- fresh
          result <-
            concat <$> nodes `forM` \case
              Increase i -> compileIncrease i
              Decrease i target -> do
                targetNode <- lift $ M.lookup target tagIndex
                compileDecrease i targetNode
              GoTo target -> do
                targetNode <- lift $ M.lookup target tagIndex
                t1 <- prev
                _ <- fresh
                pure
                  [
                    ( t1
                    , Spec
                        (Transition (SetTo B0) targetNode)
                        (Transition (SetTo B1) targetNode)
                    )
                  ]
          let portToInitial =
                ( port
                , Spec
                    (Transition (SetTo B0) initial)
                    (Transition (SetTo B1) initial)
                )
          pure $ portToInitial : result
      result :: Maybe (TM.Program Integer) = action `evalStateT` QGen 0
   in (unsafePerformIO . putStrLn . maybe "<Empty>" ((++ "\n") . programAsStr) $ result)
        `seq` result

compileDecrease :: (MonadState QGen m, MonadFail m) => Integer -> Integer -> m [(Integer, Spec Integer)]
compileDecrease i targetIfZero = do
  findi <-
    concat <$> [1 .. (i - 1)] `forM` const do
      ta <- prev
      [tb, tc] <- replicateM 2 fresh
      pure
        [ (ta, Spec (Transition (SetTo B1) ta) (Transition (MoveTo R) tb))
        , (tb, Spec (Transition (MoveTo R) tc) (Transition (MoveTo R) tb))
        ]
  checkIfZero <- do
    t1 <- prev
    [t2, t3] <- replicateM 2 fresh
    goToStandard <- do
      t1 <- prev
      [t2, t3, t4] <- replicateM 3 fresh
      pure
        [ (t1, Spec (Transition (MoveTo L) t2) (Transition (MoveTo L) t1))
        , (t2, Spec (Transition (MoveTo R) t3) (Transition (MoveTo L) t1))
        , (t3, Spec (Transition (MoveTo R) t4) Halt)
        ]
    tendSt <- prev
    tlast <- fresh
    pure $
      concat
        [
          [ (t1, Spec (Transition (SetTo B1) t1) (Transition (MoveTo R) t2))
          , (t2, Spec (Transition (MoveTo L) t3) (Transition (MoveTo R) tlast))
          ]
        , goToStandard
        ,
          [
            ( tendSt
            , Spec
                (Transition (SetTo B0) targetIfZero)
                (Transition (SetTo B1) targetIfZero)
            )
          ]
        ]
  eraseIfNeeded <- do
    t1 <- prev
    [t2, t3, t4, t5] <- replicateM 4 fresh
    pure
      [ (t1, Spec (Transition (MoveTo L) t2) (Transition (MoveTo R) t1))
      , (t2, Spec (Transition (MoveTo R) t3) (Transition (SetTo B0) t2))
      , (t3, Spec (Transition (SetTo B1) t3) (Transition (MoveTo R) t4))
      , (t4, Spec (Transition (MoveTo L) t5) (Transition (MoveTo R) t1))
      ]
  goToStandard <- do
    t1 <- prev
    [t2, t3, t4] <- replicateM 3 fresh
    pure
      [ (t1, Spec (Transition (MoveTo L) t2) (Transition (MoveTo L) t1))
      , (t2, Spec (Transition (MoveTo R) t3) (Transition (MoveTo L) t1))
      , (t3, Spec (Transition (MoveTo R) t4) Halt)
      ]
  pure $
    findi
      ++ checkIfZero
      ++ eraseIfNeeded
      ++ goToStandard

reserveTags :: (MonadState QGen m) => FlowChart Integer Integer -> m (M.Map Integer Integer)
reserveTags fc =
  M.fromList <$> fc `forM` \(Seq tag _) -> (tag,) <$> fresh

newtype QGen = QGen {unQGen :: Integer}

fresh :: (MonadState QGen m) => m Integer
fresh = modify' (QGen . (+ 1) . unQGen) >> (unQGen <$> get)

prev :: (MonadState QGen m) => m Integer
prev = unQGen <$> get

compileIncrease :: (MonadState QGen m, MonadFail m) => Integer -> m [(Integer, Spec Integer)]
compileIncrease i = do
  findBlankRightToi <-
    concat <$> [1 .. (i - 1)] `forM` const do
      ta <- prev
      [tb, tc] <- replicateM 2 fresh
      pure
        [ (ta, Spec (Transition (SetTo B1) ta) (Transition (MoveTo R) tb))
        , (tb, Spec (Transition (MoveTo R) tc) (Transition (MoveTo R) tb))
        ]
  t1 <- prev
  [t2, t3, t4, t5, t6, t7, t8, t9, t10] <- replicateM 9 fresh
  pure $
    findBlankRightToi
      ++ [ (t1, Spec (Transition (SetTo B1) t1) (Transition (MoveTo R) t2))
         , (t2, Spec (Transition (SetTo B1) t3) (Transition (MoveTo R) t2))
         , (t3, Spec Halt (Transition (MoveTo R) t4))
         , (t4, Spec (Transition (MoveTo L) t7) (Transition (SetTo B0) t5))
         , (t5, Spec (Transition (MoveTo R) t6) Halt)
         , (t6, Spec (Transition (SetTo B1) t3) (Transition (MoveTo R) t6))
         , (t7, Spec (Transition (MoveTo L) t8) (Transition (MoveTo L) t7))
         , (t8, Spec (Transition (MoveTo R) t9) (Transition (MoveTo L) t7))
         , (t9, Spec (Transition (MoveTo R) t10) Halt)
         ]
