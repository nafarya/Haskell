module Cantor
  ( gen
  ) where

import           Control.Monad (replicateM)

-- | Problem solution function which generates an infinite list
-- of tuples that comply the following constraints:
-- For ∀ M ∃ (a,b,c) : (a,b,c) ∈ "first M elements of this list"
-- This can be achieved by generating a list of all permutations
-- with repetitions of elements from 1 to ∞. Let's call this
-- number as "bound".
-- I.e. for bound=1 we have only one tuple: (1,1,1)
--      for bound=2 we have eight tuples: (1,1,1),(1,1,2),(1,2,1),...,(2,2,2)
-- Considering that for bound=n we have 3^n permutations with repititions,
-- we can estimate M for any (a,b,c) which will be somewhere between
-- lower bound L and upper bound U:
-- L = 3^(max(a, b, c)), U = 3^(max(a, b, c)+1)
gen :: [(Integer, Integer, Integer)]
gen = concat $ map perms [1..]

-- | Generates a list of all permutations with repetitions of length 3
-- with l elements
perms :: Integer -> [(Integer, Integer, Integer)]
perms l = map tuplify $ replicateM 3 [1..l]

-- | Unwrap a list of three elements to tuple
tuplify :: [a] -> (a,a,a)
tuplify [x,y,z] = (x,y,z)
