{-# LANGUAGE MultiParamTypeClasses #-}

module Physics.Contact where

import Control.Lens
import Data.Either
import Linear.Affine
import Linear.Epsilon
import Linear.V
import Linear.V2
import Linear.Vector
import Linear.Matrix
import qualified Physics.Geometry as G
import Physics.Constraint
import Physics.ConstraintSolver
import Physics.ConvexHull
import Physics.Linear
import Physics.Transform
import Utils.Utils

data Contact a = Contact { contactA :: !(PhysicalObj a)
                         , contactB :: !(PhysicalObj a)
                         , contactPoint :: !(P2 a)
                         , contactNormal :: !(V2 a)
                         , contactDepth :: !a
                         , contactIndex :: !(Int, Int) } deriving Show
data ContactBehavior a = ContactBehavior { contactBaumgarte :: !a
                                         , contactPenetrationSlop :: !a } deriving Show

class (Physical a p) => Contactable a p where
  contactMu :: p -> a
  contactHull :: p -> ConvexHull a

defaultContactBehavior :: (Num a) => ContactBehavior a
defaultContactBehavior = ContactBehavior { contactBaumgarte = 0
                                         , contactPenetrationSlop = 0 }

generateContacts :: (Epsilon a, Floating a, Ord a, Contactable a p) => (p, p) -> [Flipping (Contact a)]
generateContacts contactPair =
  case mContact of Nothing -> []
                   Just contactInfo -> flipInjectF $ flipMap f contactInfo contactPair'
  where shapes = pairMap contactHull contactPair
        contactPair' = contactPair ^. physPair
        mContact = uncurry G.contact shapes
        f (contactInfo', feat) (a', b') = fmap g points
          where points = G.flattenContactPoints contactInfo'
                n = G.contactNormal contactInfo'
                g point =
                  Contact { contactA = a'
                          , contactB = b'
                          , contactPoint = G._neighborhoodCenter point
                          , contactNormal = n
                          , contactDepth = G.contactDepth feat (G._neighborhoodCenter point)
                          , contactIndex = (G._neighborhoodIndex feat, G._neighborhoodIndex point)
                          }
