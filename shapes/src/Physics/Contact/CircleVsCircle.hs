module Physics.Contact.CircleVsCircle where

import           GHC.Types              (Double (..))

import           Data.Either
import           Physics.Contact.Circle
import           Physics.Contact.Types
import           Physics.Linear

generateContacts :: Circle -> Circle -> Maybe Contact
generateContacts = contact
