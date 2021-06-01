module Utils.QString where

newtype QString = QString Integer deriving (Eq, Ord)

instance Show QString where
  show (QString i) = "q" ++ show i
