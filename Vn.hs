{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, GADTs #-}

module Vn where

import Vector
import DepTypes

data VNil a = VNil
data VCons v a = VCons a (v a)

type V0 = VNil
type V1 = VCons V0
type V2 = VCons V1
type V3 = VCons V2
type V4 = VCons V3
type V5 = VCons V4
type V6 = VCons V5
type V7 = VCons V6
type V8 = VCons V7

instance Nat (VNil a) where
  toInt _ = 0

instance (Nat (a n)) => Nat (VCons a n) where
  toInt (VCons _ n) = 1 + toInt n

instance Functor VNil where
  fmap _ VNil = VNil

instance (Functor v) => Functor (VCons v) where
  fmap f (VCons x xs) = VCons (f x) (fmap f xs)

instance Unlistable VNil where
  fromList _ = VNil

instance (Unlistable v) => Unlistable (VCons v) where
  fromList [] = error "List too short!"
  fromList (x:xs) = VCons x (fromList xs)

instance Collection VNil where
  toList _ = []

instance (Collection v) => Collection (VCons v) where
  toList (VCons x xs) = x : toList xs 

instance (Show a) => Show (VNil a) where
  show _ = ""

instance (Show (v a), Show a) => Show (VCons v a) where
  show (VCons x xs) = show x ++ ", " ++ show xs

instance Applicative VNil where
  pure _ = VNil
  _ <*> _ = VNil

instance (Applicative v) => Applicative (VCons v) where
  pure x = VCons x (pure x)
  (VCons f fs) <*> (VCons x xs) = VCons (f x) (fs <*> xs)

instance Num a => Vector VNil a
instance (Vector v a, Num a) => Vector (VCons v) a

v1 = fromList [1, 2, 3] :: V3 Int
v2 = fromList [2, 3, 4] :: V3 Int

--main :: IO ()
--main = print (v1 + v2)