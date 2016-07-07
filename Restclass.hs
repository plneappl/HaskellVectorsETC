{-# LANGUAGE GADTs, MultiParamTypeClasses #-}

module Residueclass where
  
data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat
  deriving (Eq)

one :: Nat
one = Succ Zero

instance Num Nat where
  Zero + n = n
  n + Zero = n
  n + (Succ m) = Succ n + m
  Zero * _ = Zero
  _ * Zero = Zero
  n * (Succ m) = n + n * m
  abs n = n
  signum Zero = Zero
  signum _ = one
  negate n = n
  fromInteger 0 = Zero
  fromInteger n = Succ $ fromInteger (n - 1)

class Residueclass a where
  limit :: Nat

data Z2
instance Residueclass Z2 where
  limit = one
