{-# LANGUAGE TemplateHaskell #-}

module Physics.Scenes.Scene where

import Control.Lens
import Physics.Engine.Class
import Physics.World.Class

data Scene e =
  Scene { _scWorld :: PEWorld e (PEWorldObj e)
        , _scExts :: [External]
        , _scContactBeh :: PEContactBehavior e
        }
makeLenses ''Scene
