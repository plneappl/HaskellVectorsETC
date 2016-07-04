{-# LANGUAGE FlexibleContexts #-}

module MLD where

import Logging
import Control.Applicative
import Control.Monad
import Data.List

import VectorSpace
import V3
import V8
import Vector
import Z2

data RMCode = RM{r :: Int, m :: Int} deriving (Show)

errorCorrectivity :: RMCode -> Int
errorCorrectivity (RM r m) = (2 ^ (m-r-1)) - 1

dimension :: RMCode -> Int
dimension (RM r m) = 2 ^ (m - r)

mld :: RMCode -> V8 Z2 -> Logging (V8 Z2)
mld rm z = stage1 rm z

stage1 :: (Eq [Int]) => RMCode -> V8 Z2 -> Logging (V8 Z2)
stage1 rm@(RM r m) z = do
  let majs = map stage1_1 (vectorSpaceBases r :: [[V3 Z2]]) 
  res <- stage1_3 $ map stage1_1 (vectorSpaceBases r :: [[V3 Z2]])
  L [show majs] res where
  stage1_1 m_vs = (,) 
    (tr (chi m_vs)) $ 
    moreOne $ 
    map stage1_2 $ 
    zip [1..] $ 
    take (2^(m - r) - 2) $ 
    nubBy (\b1 b2 -> containsAll b1 b2) $ 
    filter (containsAll m_vs) $ 
    vectorSpaceBases (r+1)
  stage1_2 (i, mi) = let
    ki = (chi mi) ⋅ z in
    ki
  stage1_3 majs = stage2 1 majs rm z

stage2 :: (Eq [Int]) => Int -> [([Int], Bool)] -> RMCode -> V8 Z2 -> Logging (V8 Z2)
stage2 run majs rm@(RM r m) z | r - run <= 0 = stage3 run majs rm z
                              | otherwise    = stage2 (run + 1) (map stage2_1 (vectorSpaceBases (r - run) :: [[V3 Z2]])) rm z where
  stage2_1 n_vs = let
    nis = take (2 ^ (m - r) - 2) $ zip [1..] $ filter (containsAll n_vs) (vectorSpaceBases (r + 1 - run) :: [[V3 Z2]]) in
    (,) (tr (chi n_vs)) $ moreTrue $ map stage2_2 nis
  stage2_2 (i, ni) = findMaj majs (tr $ chi ni)

stage3 :: (Eq [Int]) => Int -> [([Int], Bool)] -> RMCode -> V8 Z2 -> Logging (V8 Z2)
stage3 run majs rm@(RM r m) z = let
  atOthers = map (findMaj majs . tr . chi) $ filter isVectorSpaceBase $ map (: []) (allVectors :: [V3 Z2])
  at1 = moreTrue atOthers
  atOthers' = at1 : (if at1 then map not atOthers else atOthers) in 
  L ["atOthers: " ++ show atOthers, "atOthers': " ++ show atOthers'] $ z + (fromBoolV $ fromList atOthers')

findMaj :: (Eq [Int]) => [([Int], Bool)] -> [Int] -> Bool
findMaj ((tr, ug):majs) v | tr == v   = ug
                          | otherwise = findMaj majs v
findMaj _ v = error ("not found:" ++ show v)

toBoolV :: (Vector v Z2) => v Z2 -> v Bool
toBoolV = fmap (toEnum . fromEnum)

fromBoolV :: (Vector v Z2) => v Bool -> v Z2
fromBoolV = fmap (toEnum . fromEnum)

moreOne :: [Z2] -> Bool
moreOne kis = (sum $ map (fromEnum :: Z2 -> Int) kis) > div (length kis) 2

moreTrue :: [Bool] -> Bool
moreTrue kis = (sum $ map (fromEnum :: Bool -> Int) kis) > div (length kis) 2

tr :: (Vector v Z2) => v Z2 -> [Int]
tr v = map fst $ filter (\(_, xi) -> xi /= 0) $ zip [1..] (toList v)

chi :: [V3 Z2] -> V8 Z2
chi vsb = fromList $ map ((toEnum :: Int -> Z2) . (fromEnum :: Bool -> Int) . (`inVectorSpace` vsb)) (allVectors :: [V3 Z2])


f :: Int -> Logging Int
f x = L {entries = [show (x, x*x)], d = x*x}

g :: Int -> Int
g x = x + 2

main :: IO ()
main = do
  let z = (V8 1 0 1 1 1 0 0 1) :: V8 Z2
  let yL = mld (RM 1 3) z
  let y = d yL
  print z
  print y
  print (z + y)
  print ""
  print yL