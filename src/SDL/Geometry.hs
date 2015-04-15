{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module SDL.Geometry where

import qualified Graphics.UI.SDL.Types as SDL.T
import Foreign.C.Types

instance Num (GeomPoint) where
   (ax, ay) + (bx, by) = (ax + bx, ay + by)
   (ax, ay) - (bx, by) = (ax - bx, ay - by)
   (ax, ay) * (bx, by) = (ax * bx, ay * by)
   abs (x, y) = (abs x, abs y)
   signum (x, y) = (signum x, signum y)
   fromInteger a = (fromInteger a, 0)


type GeomPoint = (CInt, CInt)


toRect :: (Integral a) => a -> a -> a -> a -> SDL.T.Rect
toRect x y w h = SDL.T.Rect { SDL.T.rectX = fromIntegral x, SDL.T.rectY = fromIntegral y, SDL.T.rectW = fromIntegral w, SDL.T.rectH = fromIntegral h }


moveTo :: (Integral a1, Integral a2) => SDL.T.Rect -> (a1, a2) -> SDL.T.Rect
moveTo rect (x, y) = rect { SDL.T.rectX = fromIntegral x, SDL.T.rectY = fromIntegral y }


moveBy :: (Integral a1, Integral a2) => SDL.T.Rect -> (a1, a2) -> SDL.T.Rect
moveBy shape (x, y) = shape { SDL.T.rectX = SDL.T.rectX shape + fromIntegral x, SDL.T.rectY = SDL.T.rectY shape + fromIntegral y }


centredOn :: SDL.T.Rect -> SDL.T.Rect -> SDL.T.Rect
centredOn inner outer = inner `moveBy` (centreOf outer - centreOf inner)


centreOf :: SDL.T.Rect -> GeomPoint
centreOf shape = (x, y)
    where x = SDL.T.rectX shape + SDL.T.rectW shape `div` 2
          y = SDL.T.rectY shape + SDL.T.rectH shape `div` 2


toSDLPoint :: GeomPoint -> SDL.T.Point
toSDLPoint (x, y) = SDL.T.Point { SDL.T.pointX = x, SDL.T.pointY = y }
