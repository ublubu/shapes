{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Physics.Contact.Types where

import           GHC.Generics                 (Generic)

import           Control.DeepSeq
import           Control.Lens
import           Data.Vector.Unboxed.Deriving
import           Physics.Linear

-- | Configuring contact constraint behavior
-- | TODO: does this belong in a different module?
data ContactBehavior =
  ContactBehavior { contactBaumgarte       :: !Double
                  -- ^ Bias factor: 0 <= B <= 1, used to feed positional error back into a constraint
                  , contactPenetrationSlop :: !Double
                  -- ^ Amount objects must overlap before they are considered \"touching\"
                  } deriving Show

-- | A contact between two objects - the source of a single set of contact constraints
data Contact = Contact
  { _contactNormal :: !V2
           -- ^ Unit normal of penetrated edge (or direction toward center of circle)
  , _contactCenter :: !P2
           -- ^ Coordinates of penetrating feature (or the best estimate of point-of-contact)
  , _contactDepth  :: !Double -- ^ Depth of penetration
  } deriving (Show, Generic, NFData)

makeLenses ''Contact
derivingUnbox "Contact"
  [t| Contact -> (V2, P2, Double) |]
  [| \Contact{..} -> (_contactNormal, _contactCenter, _contactDepth) |]
  [| \(n, p, d) -> Contact n p d |]
