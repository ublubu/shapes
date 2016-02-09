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
import qualified Physics.SAT as SAT
import Physics.Constraint
import Physics.ConstraintSolver
import Physics.ConvexHull
import Physics.Linear
import Physics.Transform
import Utils.Utils

-- TODO: do I need contactA and contactB? it seems like these don't matter.
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
        mContact = uncurry SAT.contact shapes
        f (contactInfo', feat) (a', b') = fmap g points
          where points = SAT.flattenContactPoints contactInfo'
                n = SAT.contactNormal contactInfo'
                g point =
                  Contact { contactA = a'
                          , contactB = b'
                          , contactPoint = _neighborhoodCenter point
                          , contactNormal = n
                          , contactDepth = SAT.contactDepth feat (_neighborhoodCenter point)
                          , contactIndex = (_neighborhoodIndex feat, _neighborhoodIndex point)
                          }
