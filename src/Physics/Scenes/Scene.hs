{-# LANGUAGE TemplateHaskell #-}

module Physics.Scenes.Scene where

import Control.Lens
import Physics.Contact
import Physics.Object
import Physics.World

data Scene p a b c = Scene { _scWorld :: World (WorldObj a)
                           , _scExts :: [External b p]
                           , _scContactBeh :: ContactBehavior c }
makeLenses ''Scene
