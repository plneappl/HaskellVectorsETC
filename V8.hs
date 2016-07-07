{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses #-}

module V8 where

import Vector

data V8 a = V8 a a a a a a a a 

instance (Show a) => Show (V8 a) where
  show (V8 x1 x2 x3 x4 x5 x6 x7 x8) = show (x1, x2, x3, x4, x5, x6, x7, x8)

instance Unlistable V8 where
  fromList (x1:x2:x3:x4:x5:x6:x7:x8:_) = V8 x1 x2 x3 x4 x5 x6 x7 x8
  fromList _ = error "List too short!"

instance Collection V8 where
  toList (V8 x1 x2 x3 x4 x5 x6 x7 x8) = [x1, x2, x3, x4, x5, x6, x7, x8]

instance (Num a) => Vector V8 a
  
instance Functor V8 where
  fmap f (V8 x1 x2 x3 x4 x5 x6 x7 x8) = V8 (f x1) (f x2) (f x3) (f x4) (f x5) (f x6) (f x7) (f x8)

instance Applicative V8 where
  pure x = V8 x x x x x x x x
  (V8 f1 f2 f3 f4 f5 f6 f7 f8) <*> (V8 x1 x2 x3 x4 x5 x6 x7 x8) = V8 (f1 x1) (f2 x2) (f3 x3) (f4 x4) (f5 x5) (f6 x6) (f7 x7) (f8 x8)










