module TuringMachines.PPrint where

import TuringMachines.Core
import TuringMachines.Numbering (allStates)
import TuringMachines.Show ()

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Utils (tshow)

pprint :: (Ord a, Show a) => Program a -> Text
pprint program =
  T.intercalate "\n"
    . fmap
      ( \(q, spec) ->
          T.unwords
            [ T.justifyLeft maxStateLen ' ' (tshow q)
            , pprintSpec maxStateTransitionLen spec
            ]
      )
    . M.toAscList
    $ program
  where
    maxStateLen = M.foldlWithKey' (\a k _ -> max a (length . show $ k)) 0 program
    maxStateTransitionLen =
      L.foldl' max 0
        . fmap (length . show)
        . S.toAscList
        $ allStates program

pprintSpec :: Show a => Int -> Spec a -> Text
pprintSpec maxLen (Spec if0 if1) =
  T.unwords
    [ T.justifyLeft (maxLen + 1) ' ' (pprintTransition if0)
    , T.stripEnd $ pprintTransition if1
    ]

pprintTransition :: Show a => Transition a -> Text
pprintTransition (Transition a q) = T.concat [pprintAction a, tshow q]
pprintTransition Halt = " _"

pprintAction :: Action -> Text
pprintAction a = case a of
  (SetTo B0) -> "0"
  (SetTo B1) -> "1"
  (MoveTo L) -> "L"
  (MoveTo R) -> "R"

pprintBit :: Bit -> Text
pprintBit b = case b of
  B0 -> "0"
  B1 -> "1"
