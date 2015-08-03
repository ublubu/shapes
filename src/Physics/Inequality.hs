module Physics.Inequality where

import Control.Lens
import Physics.Constraint hiding (solveConstraint)
import qualified Physics.Constraint as C
import Utils.Utils

data ThanZero = LessThan | GreaterThan

zeroTest :: (Ord a, Num a) => ThanZero -> a -> a -> Bool
zeroTest LessThan slop a = a < slop
zeroTest GreaterThan slop a = a > (-slop)

-- arg0 is total, arg1 is new increment
type Clamper a = a -> a -> (a, a) -- fst is the clamped increment, snd is the new total
data Clamped n a = Clamped n a

clamplicator :: (Physical a n, Fractional n) => Clamper n -> Constraint' n -> Clamped n (a, a) -> Clamped n (a, a)
clamplicator f c' (Clamped l_acc cp) = Clamped l_acc' (overWith physObj physObj g cp)
  where c = c' cp'
        lagr = lagrangian2 cp' c
        cp' = pairMap (view physObj) cp
        (dl, l_acc') = f l_acc lagr
        g = applyLagrangian2' dl c
