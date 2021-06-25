{-# LANGUAGE StrictData #-}

module Abacus.Core where

data Node tag a = Increase a | Decrease a tag | GoTo tag deriving (Eq, Ord, Show)
data Seq tag a = Seq tag [Node tag a] deriving (Eq, Ord, Show)
type FlowChart tag a = [Seq tag a]
