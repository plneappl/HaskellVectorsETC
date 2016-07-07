{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, FlexibleInstances, UndecidableInstances #-}

module Vector where

import Control.Applicative

class Unlistable v where
  fromList :: [a] -> v a
  
class Unlistable v => Collection v where
  toList :: v a -> [a]

instance {-# OVERLAPPABLE #-} Collection v => Foldable v where
  foldr f x0 v = foldr f x0 (toList v)
  
--instance (Foldable v, Unlistable v) => Collection v where
--  toList = foldr (:) []

vPlus :: (Vector v a) => v a -> v a -> v a
vPlus = liftA2 (+)

class (Applicative v, Collection v, Num a) => Vector v a where
  (*^) :: a -> v a -> v a
  (^*) :: v a -> a -> v a
  k *^ v = pure k * v
  (^*) = flip (*^)
  (⋅) :: (Num a) => v a -> v a -> a
  v ⋅ w = sum $ toList $ v * w
  unitV :: Int -> v a
  unitV i = fromList $ map fromInteger $ (replicate (i - 1) 0) ++ [1] ++ [0, 0..]
  
instance {-# OVERLAPPABLE #-} (Eq a, Vector v a) => Eq (v a) where
  v == w = and (zipWith (==) (toList v) (toList w)) 

instance (Vector v a) => Num (v a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger
  negate = fmap negate

instance (Enum a, Bounded a, Vector v a) => Enum (v a) where
  toEnum i 
    | i < 0     = toEnum (-i)
    | otherwise = let
      maxI = fromEnum (maxBound :: a)
      minI = fromEnum (minBound :: a)
      cnt = maxI - minI + 1 
      elems = [div i (cnt ^ pow) | pow <- [0..]] in
      ((toEnum :: Int -> a) <$> (fromList :: [Int] -> v Int) elems)

  fromEnum (v :: v a) = let
    elems = toList $ fmap (fromEnum :: a -> Int) v 
    maxI = (fromEnum :: a -> Int) (maxBound :: a) :: Int 
    minI = (fromEnum :: a -> Int) (minBound :: a) :: Int
    cnt = maxI - minI + 1 in
    sum $ map (\(e, pow) -> e * (cnt ^ pow)) $ elems `zip` [0..]

instance (Bounded a, Vector v a) => Bounded (v a) where
  minBound = pure minBound
  maxBound = pure maxBound

allVectors :: forall a v. (Bounded (v a), Enum (v a), Vector v a) => [v a]
allVectors = let
  minI = fromEnum (minBound :: v a)
  maxI = fromEnum (maxBound :: v a) in
  map toEnum [minI .. maxI]


