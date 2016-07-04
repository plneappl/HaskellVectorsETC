{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module V3 where

import Vector

data V3 a = V3 a a a

instance Unlistable V3 where
  fromList (x:y:z:_) = V3 x y z
  fromList _ = error "List too short!"

instance Collection V3 where
  toList (V3 x y z) = [x, y, z]

instance (Show a) => Show (V3 a) where
  show (V3 x1 x2 x3) = show (x1, x2, x3)

instance Functor V3 where
  fmap f (V3 x1 x2 x3) = V3 (f x1) (f x2) (f x3)

instance Applicative V3 where
  pure x = V3 x x x
  (V3 f1 f2 f3) <*> (V3 x1 x2 x3) = V3 (f1 x1) (f2 x2) (f3 x3)

instance (Num a) => Vector V3 a

(×) :: (Num a) => V3 a -> V3 a -> V3 a
(V3 x1 x2 x3) × (V3 y1 y2 y3) = V3 (x2*y3 - x3*y2) (x3*y1 - x1*y3) (x1*y2 - x2*y1) 

