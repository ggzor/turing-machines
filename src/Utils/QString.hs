module Utils.QString where

import GHC.Exts

newtype QString = QString Integer deriving (Eq, Ord)

instance Show QString where
  show (QString i) = "q" ++ show i

instance IsString QString where
  fromString ('q' : number) = QString $ read number
