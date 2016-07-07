{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}

module DepTypes where

data Zero = Zero   deriving Show
data Suc n = Suc n deriving Show  



class Nat n where
  toInt :: n -> Int

instance Nat Zero where
  toInt Zero = 0

instance forall n. (Nat n) => Nat (Suc n) where
  toInt (Suc n) = 1 + toInt n

--main :: IO ()
--main = print (toInt (Suc Zero))
