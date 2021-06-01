module Math.Primes where

import Data.Bifunctor (first)

primes :: [Integer]
primes = 2 : sieve primes [3 ..]
  where
    sieve (p : pt) xs =
      let (h, t) = span (< p * p) xs
       in h ++ sieve pt [x | x <- t, rem x p > 0]
    sieve [] _ = []

factorize :: Integer -> [Integer]
factorize x = go x primes
  where
    go n ps@(p : t)
      | p * p > n = [n]
      | r == 0 = p : go q ps
      | otherwise = go n t
      where
        (q, r) = quotRem n p
    go _ [] = []

primeExp :: Integer -> Integer -> (Integer, Integer)
primeExp n p
  | r == 0 = first (+ 1) (primeExp q p)
  | otherwise = (0, n)
  where
    (q, r) = quotRem n p

countPrimes :: Integer -> [(Integer, Integer)]
countPrimes x = go x primes
  where
    go n (p : t)
      | n == 1 = []
      | otherwise = (p, count) : go rest t
      where
        (!count, !rest) = primeExp n p
    go _ [] = []
