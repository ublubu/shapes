{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Physics.Contact where

import           GHC.Generics                   (Generic)

import           Control.DeepSeq
import           Physics.Contact.Circle
import qualified Physics.Contact.CircleVsCircle as CC
import qualified Physics.Contact.CircleVsHull   as CH
import           Physics.Contact.ConvexHull
import qualified Physics.Contact.HullVsHull     as HH
import           Physics.Contact.Types
import           Physics.Linear
import           Utils.Descending
import           Utils.Utils

data Shape = HullShape ConvexHull | CircleShape Circle
  deriving (Show, Generic, NFData)

generateContacts ::
  (Shape, Shape)
  -> Descending ((Int, Int), Flipping Contact)
generateContacts (CircleShape a, CircleShape b) =
  Descending $
  case CC.generateContacts a b of
    Nothing      -> []
    Just contact -> [((0, 0), Same contact)]
generateContacts (CircleShape a, HullShape b) =
  Descending $
  case CH.generateContacts a b of
    Nothing                     -> []
    Just (hullFeature, contact) -> [((0, hullFeature), Same contact)]
generateContacts (HullShape a, CircleShape b) =
  Descending $
  case CH.generateContacts b a of
    Nothing                     -> []
    Just (hullFeature, contact) -> [((hullFeature, 0), Flip contact)]
generateContacts (HullShape a, HullShape b) = HH.generateContacts (a, b)

-- assumes scale-invariant transform from localspace
setShapeTransform :: Shape -> (P2 -> P2) -> Shape
setShapeTransform (HullShape hull) = HullShape . setHullTransform hull
setShapeTransform (CircleShape circle) = CircleShape . setCircleTransform circle
