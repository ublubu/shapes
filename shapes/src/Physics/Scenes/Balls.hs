module Physics.Scenes.Balls where

import           Control.Lens
import           Control.Monad
import           Control.Monad.ST

import           Physics.Constraint
import           Physics.Contact.Types
import           Physics.Engine
import           Physics.Scenes.Scene
import           Physics.Scenes.Stacks         (box, boxFloor', boxStack,
                                                contactBehavior, externals)
import           Physics.Scenes.TwoFlyingBoxes (boxB')
import           Physics.World

circle' :: Double
     -> (Double, Double)
     -> (Double, Double)
     -> label
     -> WorldObj label
circle' radius center velocity =
  makeWorldObj (box center velocity) 0.2 0.5 (makeCircle radius)

circleStack :: Double -- ^ radius
         -> (Double, Double) -- ^ bottom position
         -> (Double, Double) -- ^ velocity
         -> Double -- ^ vertical spacing
         -> Int -- ^ number of objects
         -> label -- ^ arbitrary user data
         -> [WorldObj label]
circleStack _ _ _ _ 0 _ = []
circleStack diameter bottom vel spacing n ext =
  circle' (diameter / 2) bottom vel ext : circleStack diameter bottom' vel spacing (n - 1) ext
  where bottom' = bottom & _2 %~ (+ (diameter + spacing))

stacks_ :: (Bool -> Bool)
       -> Double
       -> (Double, Double)
       -> (Double, Double)
       -> Double
       -> (Int, Int)
       -> label
       -> [WorldObj label]
stacks_ ftype diameter (center, bottom) vel spacing (n_w, n_h) ext =
  join . fmap f . take n_w $ iterate (\(a, b) -> (a + diameter, ftype b)) (leftmost, True)
  where leftmost = center - (diameter * fromIntegral (n_w - 1) / 2)
        f (left, True) = circleStack diameter (left, bottom) vel spacing n_h ext
        f (left, False) = boxStack (diameter, diameter) (left, bottom) vel spacing n_h ext

stacks :: Double
       -> (Double, Double)
       -> (Double, Double)
       -> Double
       -> (Int, Int)
       -> label
       -> [WorldObj label]
stacks = stacks_ not

stacks' :: Double
       -> (Double, Double)
       -> (Double, Double)
       -> Double
       -> (Int, Int)
       -> label
       -> [WorldObj label]
stacks' = stacks_ (const True)

makeScene :: (Int, Int) -> Double -> Double -> label -> ST s (Scene s label)
makeScene dims diameter spacing ext = do
  w <- makeWorld (boxFloor' ext : stacks diameter (0, -4.5) (0, 0) spacing dims ext)
  return $ Scene w externals contactBehavior

makeScene' :: (Int, Int) -> Double -> Double -> label -> ST s (Scene s label)
makeScene' dims diameter spacing ext = do
  w <- makeWorld (boxFloor' ext : stacks' diameter (0, -4.5) (0, 0) spacing dims ext)
  return $ Scene w externals contactBehavior

circleA :: PhysicalObj
circleA = makePhysicalObj (1, 0) 0 (-5, 0) 0 (2, 1)

circleB :: PhysicalObj
circleB = makePhysicalObj (-4, 0) 0 (5, 1.5) 0 (1, 0.5)

circleA' :: label -> WorldObj label
circleA' = makeWorldObj circleA 0.2 1 $ makeCircle 2

circleB' :: label -> WorldObj label
circleB' = makeWorldObj circleB 0.2 1 $ makeCircle 1

twoCircles :: label -> label -> ST s (Scene s label)
twoCircles a b = do
  world <- makeWorld [circleA' a, circleB' b]
  return $ Scene world (const id) contactBehavior

circleAndBox :: label -> label -> ST s (Scene s label)
circleAndBox a b = do
  world <- makeWorld [circleA' a, boxB' b]
  return $ Scene world (const id) contactBehavior
