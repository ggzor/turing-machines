module Data.List.Utils where

map2 :: (a -> a -> b) -> [a] -> [b]
map2 f (x : y : xs) = f x y : map2 f xs
map2 _ _ = []
