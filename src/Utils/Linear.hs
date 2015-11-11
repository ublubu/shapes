module Utils.Linear where

data P2 = P2 {-# UNPACK #-} !Double
             {-# UNPACK #-} !Double

data D2 = D2 {-# UNPACK #-} !Double
             {-# UNPACK #-} !Double

afdot :: P2 -> D2 -> Double
afdot (P2 x y) (D2 x' y') = (x * x') + (y * y')

