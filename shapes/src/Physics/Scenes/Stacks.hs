module Physics.Scenes.Stacks where

import           Control.Lens
import           Control.Monad
import           Control.Monad.ST

import           Physics.Constraint
import           Physics.Contact.Types
import           Physics.Engine
import           Physics.Scenes.Scene
import           Physics.World

box :: (Double, Double)
    -> (Double, Double)
    -> PhysicalObj
box (x, y) (vx, vy) =
  makePhysicalObj (vx, vy) 0 (x, y) 0 (2, 1)

boxFloor :: PhysicalObj
boxFloor = makePhysicalObj (0, 0) 0 (0, -6) 0 (0, 0)

box' :: (Double, Double)
     -> (Double, Double)
     -> (Double, Double)
     -> label
     -> WorldObj label
box' (w, h) center velocity =
  makeWorldObj (box center velocity) 0.2 0 (makeRectangleHull w h)

boxFloor' :: label -> WorldObj label
boxFloor' =
  makeWorldObj boxFloor 0.2 0 (makeRectangleHull 18 1)

boxStack :: (Double, Double)
         -> (Double, Double)
         -> (Double, Double)
         -> Double
         -> Int
         -> label
         -> [WorldObj label]
boxStack _ _ _ _ 0 _ = []
boxStack size@(_, h) bottom vel spacing n ext =
  box' size bottom vel ext : boxStack size bottom' vel spacing (n - 1) ext
  where bottom' = bottom & _2 %~ (+ (h + spacing))

stacks :: (Double, Double)
       -> (Double, Double)
       -> (Double, Double)
       -> Double
       -> (Int, Int)
       -> label
       -> [WorldObj label]
stacks size@(w, _) (center, bottom) vel spacing (n_w, n_h) ext =
  join . fmap f . take n_w $ iterate (+ w) leftmost
  where leftmost = center - (w * fromIntegral (n_w - 1) / 2)
        f left = boxStack size (left, bottom) vel spacing n_h ext

world :: label -> ST s (World s label)
world ext =
  makeWorld $
  concat
    [ [boxFloor' ext]
    , boxStack (2, 2) (8, -4.5) (-1, 0) 0 5 ext
    , boxStack (2, 2) (5.5, -4.5) (-2, 0) 0 5 ext
    ]

world' :: label -> ST s (World s label)
world' ext =
  makeWorld $
  concat
    [ [boxFloor' ext]
    , boxStack (2, 2) (0, -4.5) (0, 0) 0 5 ext
    , [box' (2, 2) (8, 0) (-6, 0) ext]
    ]

world'' :: label -> ST s (World s label)
world'' ext =
  makeWorld (boxFloor' ext : stacks (1, 1) (0, -4.5) (0, 0) 1 (10, 10) ext)

world''' :: label -> ST s (World s label)
world''' ext =
  makeWorld (boxFloor' ext : stacks (0.75, 0.75) (0, -4.5) (0, 0) 1 (15, 15) ext)

externals :: External
externals = makeConstantAccel (0, -2)

contactBehavior :: ContactBehavior
contactBehavior = ContactBehavior 0.01 0.02

scene :: label -> ST s (Scene s label)
scene ext = do
  w <- world ext
  return $ Scene w externals contactBehavior

scene' :: label -> ST s (Scene s label)
scene' ext = do
  w <- world' ext
  return $ Scene w externals contactBehavior

scene'' :: label -> ST s (Scene s label)
scene'' ext = do
  w <- world'' ext
  return $ Scene w externals contactBehavior

scene''' :: label -> ST s (Scene s label)
scene''' ext = do
  w <- world''' ext
  return $ Scene w externals contactBehavior

makeScene :: (Int, Int) -> Double -> label -> ST s (Scene s label)
makeScene dims spacing ext = do
  w <- makeWorld (boxFloor' ext : stacks (0.2, 0.2) (0, -4.5) (0, 0) spacing dims ext)
  return $ Scene w externals contactBehavior
