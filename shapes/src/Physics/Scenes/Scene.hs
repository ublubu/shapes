{-# LANGUAGE TemplateHaskell #-}

module Physics.Scenes.Scene where

import Control.Lens
import Physics.Engine.Class

data Scene e =
  Scene { _scWorld :: PEWorld e (PEWorldObj e)
        , _scExts :: [PEExternal' e]
        , _scContactBeh :: PEContactBehavior e
        }
makeLenses ''Scene
