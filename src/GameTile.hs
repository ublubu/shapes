module GameTile where

import Directional
import Grid
import SlidingGrid

data TileRole = SpawnTile | GoalTile | PathTile | NothingTile deriving (Show, Eq)

data GameTile = GameTile (Rectangular Bool) TileRole deriving Show

spawnAt :: GameTile -> GameTile
spawnAt (GameTile rect _) = GameTile rect SpawnTile

goalAt :: GameTile -> GameTile
goalAt (GameTile rect _) = GameTile rect GoalTile

nothingAt :: GameTile -> GameTile
nothingAt (GameTile rect _) = GameTile rect NothingTile

tileNone :: GameTile
tileNone = GameTile (Rectangular False False False False) PathTile

tileE :: GameTile
tileE = GameTile (Rectangular True False False False) PathTile

tileS :: GameTile
tileS = GameTile (Rectangular False True False False) PathTile

tileW :: GameTile
tileW = GameTile (Rectangular False False True False) PathTile

tileN :: GameTile
tileN = GameTile (Rectangular False False False True) PathTile

tileEW :: GameTile
tileEW = GameTile (Rectangular True False True False) PathTile

tileNS :: GameTile
tileNS = GameTile (Rectangular False True False True) PathTile

tileNE :: GameTile
tileNE = GameTile (Rectangular True False False True) PathTile

tileSE :: GameTile
tileSE = GameTile (Rectangular True True False False) PathTile

tileNW :: GameTile
tileNW = GameTile (Rectangular False False True True) PathTile

tileSW :: GameTile
tileSW = GameTile (Rectangular False True True False) PathTile

tileNotN :: GameTile
tileNotN = GameTile (Rectangular True True True False) PathTile

tileNotE :: GameTile
tileNotE = GameTile (Rectangular False True True True) PathTile

tileNotS :: GameTile
tileNotS = GameTile (Rectangular True False True True) PathTile

tileNotW :: GameTile
tileNotW = GameTile (Rectangular True True False True) PathTile

tileAll :: GameTile
tileAll = GameTile (Rectangular True True True True) PathTile

