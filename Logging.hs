{-# LANGUAGE MultiParamTypeClasses #-}

module Logging(Logging(L), entries, d) where

import Control.Applicative
import Control.Monad

data Logging a = L {
  entries :: [String],
  d :: a
} 


instance (Show a) => Show (Logging a) where
  show l = show (entries l) ++ "\n" ++ show (d l)  

instance Functor Logging where
  fmap f l = l {d = f (d l)}

instance Applicative Logging where
  fl1 <*> l2 = l2 {entries = entries l2 ++ entries fl1, d = d fl1 (d l2)}
  pure x = L {entries = [], d = x}

instance Monad Logging where
  l1 >>= fl2 = let
    log1 = entries l1
    d1 = d l1 
    l2 = fl2 d1
    log2 = entries l2 
    d2 = d l2 in
    L {entries = log1 ++ log2, d = d2}
  return = pure

f :: Int -> Logging Int
f x = L {entries = [show (x, x*x)], d = x*x}

g :: Int -> Int
g x = x + 2

--main :: IO ()
--main = do
--  let res = g <$> f 2 >>= f
--  print res
--  print (d res)
