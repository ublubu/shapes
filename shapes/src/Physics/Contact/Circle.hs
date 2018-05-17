{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Physics.Contact.Circle where

import           GHC.Generics    (Generic)

import           Control.DeepSeq
import           Physics.Linear

data Circle = Circle
  { _circleCenter :: !P2 -- ^ in world coordinates
  , _circleRadius :: !Double
  } deriving (Show, Generic, NFData)

circleWithRadius :: Double -> Circle
circleWithRadius = Circle (P2 (V2 0.0## 0.0##))

-- TODO: use the same type as Physics.Contact
data Contact = Contact
  { _contactCenter :: !P2
  , _contactDepth  :: !Double
  , _contactNormal :: !V2
  } deriving (Show, Generic, NFData)

{- |
The normal points out of the "penetrated" circle.
-}
contact :: Circle
  -- ^ the penetratee
  -> Circle
  -- ^ the penetrator
  -> Maybe Contact
contact circleA circleB
  | rab * rab >= abSq =
    Just Contact {_contactCenter = center, _contactDepth = depth, _contactNormal = abN}
  | otherwise = Nothing
  where
    a = _circleCenter circleA
    b = _circleCenter circleB
    ab = diffP2 b a
    -- ^ vector from 'a' to 'b'
    ra = _circleRadius circleA
    rb = _circleRadius circleB
    rab = ra + rb
    abSq = sqLengthV2 ab
    -- ^ squared length of 'ab'
    abLength = sqrt abSq
    abN = abLength `sdivV2` ab
    -- ^ normalized 'ab'
    a' = (ra `smulV2` abN) `vplusP2` a
    -- ^ edge of circle A
    b' = ((-rb) `smulV2` abN) `vplusP2` b
    -- ^ edge of circle B
    center = midpointP2 a' b'
    depth = ra + rb - abLength

-- assumes scale-invariant transform from localspace
setCircleTransform :: Circle
  -> (P2 -> P2)
  -> Circle
setCircleTransform Circle {..} fromLocalSpace =
  Circle (fromLocalSpace zeroP2) _circleRadius
