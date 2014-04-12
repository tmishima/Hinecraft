--{-# LANGUAGE BangPatterns #-}
--
-- Copyright : (c) T.Mishima 2014
-- License : Apache-2.0
--
module Hinecraft.Types where

version :: String
version = "0.2.0"

data ChunkParam = ChunkParam
  { blockSize :: Int
  , blockNum  :: Int
  }

chunkParam :: ChunkParam
chunkParam = ChunkParam
  { blockSize = 16
  , blockNum = 8
  }

type BlockIDNum = Int

data Shape = Cube | Half Bool | Stairs
  deriving (Eq,Show,Ord)

type BlockCatalog = [BlockIDNum]

data InventoryParam = InventoryParam
  { dlgTexturePath :: FilePath
  , tabTexturePath :: FilePath
  , iconSize :: Double
  , projectionRate :: Double
  , rectDotSize :: (Int,Int)
  , uvOrg :: (Double,Double)
  , uvRectSize :: (Double,Double)
  , iconListOrg :: (Double,Double)
  , iconListItvl :: Double
  , palletOrg :: (Double,Double)
  }

inventoryParam :: InventoryParam
inventoryParam = InventoryParam 
  { dlgTexturePath =
     "/.Hinecraft/textures/gui/container/creative_inventory/tab_items.png"
  , tabTexturePath = 
     "/.Hinecraft/textures/gui/container/creative_inventory/tabs.png"
  , iconSize = 16.0
  , projectionRate = 2.5
  , rectDotSize = (w,h)
  , uvOrg = (0,0)
  , uvRectSize = (fromIntegral w / 256,fromIntegral h / 256)
  , iconListOrg = (9, fromIntegral h - 35)
  , iconListItvl = 18 
  , palletOrg = (9,fromIntegral h - 129)
  }
  where
    (w,h) = (196,136)

data UserStatus = UserStatus
  { userPos :: Pos' 
  , userRot :: Rot' 
  , palletIndex :: Int
  , userVel :: (Double,Double,Double)
  }
  deriving (Eq,Show)

data TitleModeState = TitleModeState
  { rotW :: Double 
  , isModeChgBtnEntr :: Bool 
  , isExitBtnEntr :: Bool
  , isQuit :: Bool
  }
  deriving (Eq,Show)

type DragDropMode = Maybe BlockIDNum
type DragDropState = Maybe ((Double,Double),BlockIDNum)

data PlayModeState = PlayModeState
  { usrStat :: UserStatus
  , drgdrpMd :: DragDropMode
  , drgSta ::  DragDropState
  , curPos :: Maybe (WorldIndex,Surface)
  , pallet :: [BlockIDNum]
  }
  deriving (Show)

type WorldIndex = (Int,Int,Int)
type Pos' = (Double,Double,Double)
type Rot' = Pos'
type Vel' = Pos'

type SurfacePos = [(WorldIndex,BlockIDNum,[Surface])]
data Surface = STop | SBottom | SRight | SLeft | SFront | SBack 
  deriving (Ord,Show,Eq,Read)

type BlockNo = Int
type ChunkNo = Int
type LightNo = Int
type Bright = Int
