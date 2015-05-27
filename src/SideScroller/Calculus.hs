module SideScroller.Calculus where

import Control.Applicative

data Rate a = Rate a
data RateValue a b = RateValue a b
data BaseValue a = BaseValue a

type Pos a = BaseValue a
type Speed a = RateValue () a
type Accel a = RateValue (Rate ()) a

integral :: (Num a) => RateValue () a -> a -> BaseValue a
integral (RateValue _ x') dt = BaseValue (x' * dt)

integral' :: (Num a) => RateValue (Rate b) a -> a -> RateValue b a
integral' (RateValue (Rate r) x'') dt = RateValue r (x'' * dt)

derivative :: (Fractional a) => BaseValue a -> a -> RateValue () a
derivative (BaseValue x) dt = RateValue () (x / dt)

derivative' :: (Fractional a) => RateValue b a -> a -> RateValue (Rate b) a
derivative' (RateValue r x') dt = RateValue (Rate r) (x' / dt)

class Unit u where
  unit :: u

instance Unit a => Unit (Rate a) where
  unit = Rate unit

instance Unit () where
  unit = ()

instance Functor (RateValue a) where
  fmap f (RateValue r x') = RateValue r (f x')

instance Unit a => Applicative (RateValue a) where
  pure = RateValue unit
  (RateValue _ f) <*> (RateValue _ x') = RateValue unit (f x')

instance Functor BaseValue where
  fmap f (BaseValue x) = BaseValue (f x)

instance Applicative BaseValue where
  pure = BaseValue
  (BaseValue f) <*> (BaseValue x) = BaseValue (f x)

aplus :: (Applicative t, Num a) => t a -> t a -> t a
aplus = liftA2 (+)

aminus :: (Applicative t, Num a) => t a -> t a -> t a
aminus = liftA2 (-)

amult :: (Applicative t, Num a) => t a -> t a -> t a
amult = liftA2 (*)

adiv :: (Applicative t, Fractional a) => t a -> t a -> t a
adiv = liftA2 (/)

ffloor :: (Functor t, RealFrac a, Integral b) => t a -> t b
ffloor = fmap floor

baseValue :: BaseValue a -> a
baseValue (BaseValue x) = x

speedValue :: Speed a -> a
speedValue (RateValue () x) = x

accelValue :: Accel a -> a
accelValue (RateValue (Rate ()) x) = x

toBase :: a -> BaseValue a
toBase = BaseValue

toSpeed :: a -> Speed a
toSpeed = RateValue ()

toAccel :: a -> Accel a
toAccel = RateValue (Rate ())

