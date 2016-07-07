module Z3 where

data Z3 = Zero | One | Two deriving(Bounded, Eq)

instance Show Z3 where
  show = show . fromEnum

instance Num Z3 where
  Zero + Zero   = Zero
  Zero + One    = One
  Zero + Two    = Two
  One  + One    = Two
  One  + Two    = Zero
  Two  + Two    = One
  x    + y      = y + x

  Zero * _      = Zero
  One  * x      = x
  Two  * Two    = One
  x    * y      = y * x
  abs x         = x
  signum Zero   = Zero
  signum One    = One
  signum Two    = One
  fromInteger x | mod x 3 == 0 = Zero
                | mod x 3 == 1 = One
                | otherwise    = Two
  negate One    = Two
  negate Two    = One
  negate Zero   = Zero

instance Enum Z3 where
  fromEnum Zero = 0
  fromEnum One  = 1
  fromEnum Two  = 2
  toEnum = fromInteger . fromIntegral 
