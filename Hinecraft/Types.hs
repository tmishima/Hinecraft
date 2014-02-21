module Hinecraft.Types where

data BlockID = AirBlockID | StoneBlockID | DirtBlockID
             | GlassBlockID | WoodBlockID | GrassBlockID
             | GlowBlockID | PlankBlockID | StonebrickBlockID
             | PlankHalfBlockID | CobbleStoneBlockID 
             | GravelBlockID | SandBlockID | BrickBlockID
             | LeavesBlockID | RedWoolBlockID | BlueWoolBlockID
             | OutOfRange
  deriving (Eq,Show,Ord)

data Shape = Cube | Half Bool | Stairs
  deriving (Eq,Show,Ord)

type BlockCatalog = [BlockID]

blockCatalog :: BlockCatalog
blockCatalog = [ StoneBlockID, DirtBlockID, GlassBlockID
               , WoodBlockID, GrassBlockID, GlowBlockID
               , PlankBlockID, StonebrickBlockID, PlankHalfBlockID
               , CobbleStoneBlockID, GravelBlockID, SandBlockID
               , BrickBlockID, LeavesBlockID, RedWoolBlockID
               , BlueWoolBlockID]

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


type WorldIndex = (Int,Int,Int)
type Pos' = (Double,Double,Double)
type Rot' = Pos'
type Vel' = Pos'

type SurfacePos = [(WorldIndex,BlockID,[(Surface,Bright)])]
data Surface = STop | SBottom | SRight | SLeft | SFront | SBack 
  deriving (Ord,Show,Eq)

type BlockNo = Int
type ChunkNo = Int
type LightNo = Int
type Bright = Int
