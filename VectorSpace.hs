{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses #-}

module VectorSpace where

import V3
import V8
import Vector
import Data.List


combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs']

-- P(M) = [[], [m_1], [m_2], ..., [m_1, m_2], [m_1, m_3], ...]
-- ==> M choose 0, M choose 1, ..., M choose |M|.
subsets :: [a] -> [[a]]
subsets xs = concatMap (`combinations` xs) [0..(length xs)]

-- group: groups into lists of same elements, no duplicates iff those are all of length <=1
containsNoDuplicates :: Eq a => [a] -> Bool
containsNoDuplicates as = length (nub as) == length as

allMultiples :: (Enum a, Bounded a, Vector v a) => v a -> [v a]
allMultiples (v :: v a) = let
  minI = fromEnum (minBound :: a)
  maxI = fromEnum (maxBound :: a)
  cnt = maxI - minI in
  [toEnum i *^ v | i <- [1..cnt]]


-- vs is a base iff all linearcombinations are different
-- all linearcombinations are the linearcombination with all coefficients = 1 for all subsets
-- since sum [] returns 0, this implicitly checks for that vector as well
isVectorSpaceBase :: (Enum a, Bounded a, Eq (v a), Vector v a) => [v a] -> Bool
isVectorSpaceBase (vs :: [v a]) = let 
  subs = map allMultiples vs :: [[v a]]
  lcs  = linearcombinations subs in
  --                  check for v1 == v2    check for (0, ..., 0)
  containsNoDuplicates lcs && (pure 0 `notElem` lcs)

linearcombinations :: (Vector v a) => [[v a]] -> [v a]
linearcombinations [] = []
linearcombinations (xs:xss) = xs ++ [x `vPlus` y | x <- xs, y <- linearcombinations xss]

vectorSpaceBases :: (Enum a, Enum (v a), Bounded (v a), Bounded a, Eq (v a), Vector v a) => Int -> [[v a]]
vectorSpaceBases 0 = [[]]
vectorSpaceBases n = filter isVectorSpaceBase [v:vs | v <- allVectors, vs <- vectorSpaceBases (n - 1)]

inVectorSpace :: (Enum a, Enum (v a), Bounded (v a), Bounded a, Eq (v a), Vector v a) => v a -> [v a] -> Bool
inVectorSpace v vs = not $ (v `notElem` vs) && isVectorSpaceBase (v : vs)

containsAll :: (Enum a, Enum (v a), Bounded (v a), Bounded a, Eq (v a), Vector v a) => [v a] -> [v a] -> Bool
containsAll vs ws = all (`inVectorSpace` ws) vs 

