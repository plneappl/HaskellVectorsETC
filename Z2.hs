module Z2 where

data Z2 = Zero | One deriving(Show, Bounded, Eq)
instance Num Z2 where
  Zero + Zero   = Zero
  One  + One    = Zero
  _    + _      = One
  One  * One    = One
  _    * _      = Zero
  abs x         = x
  signum Zero   = Zero
  signum One    = One
  fromInteger x | mod x 2 == 0 = Zero
                | otherwise    = One
  negate x      = x

instance Enum Z2 where
  fromEnum Zero = 0
  fromEnum One  = 1
  toEnum = fromInteger . fromIntegral 
