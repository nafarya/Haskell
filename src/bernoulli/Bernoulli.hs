module Bernoulli
  ( bernoulli
  ) where

import           Data.Ratio

-----------
-- Solution

-- | Memoized function that return Nth number of Bernoulli sequence
-- Memoization is done through adding a "caching" array which
-- stores a value once it's calculated and returning it if it's
-- already present in the array.
-- Run "stack test :bernoulli-test" to see execution times
bernoulli :: Int -> Rational
bernoulli = (map b' [0 ..] !!) where
b' 0 = 1
b' n
 | n < 0 = undefined
 | n == 1 = (-1)%2
 | odd n = 0
 | otherwise = g n

-- | Recursive definition of n'th Bernoulli number
g :: Integer -> Rational
g n = first * sum (zipWith (*) binoms bernoullis) where
  first = (-1)%(n+1)
  binoms = map (\k -> binom (n+1) (k+1)) [1..n]
  bernoullis = map (\k -> bernoulli (fromIntegral (n-k))) [1..n]

-------------------
-- Helper functions

-- Returns binomial coefficient as a rational number
binom :: Integer -> Integer -> Rational
binom n k = (fact n) % (fact k * fact (n - k))

-- | Returns n! (factorial of n)
fact :: Integer -> Integer
fact 0 = 1
fact 1 = 1
fact n = n * fact (n - 1)

