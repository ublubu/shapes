{-# LANGUAGE TemplateHaskell #-}

module Physics.Scenes.Scene where

import Control.Lens
import Physics.Contact
import Physics.Object
import Physics.World

data Scene a p = Scene { _scWorld :: World (WorldObj a)
                       , _scExts :: [External a p]
                       , _scContactBeh :: ContactBehavior a }
makeLenses ''Scene
