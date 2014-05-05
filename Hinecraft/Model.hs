--{-# LANGUAGE BangPatterns #-}
--
-- Copyright : (c) T.Mishima 2014
-- License : Apache-2.0
--
module Hinecraft.Model where
import Hinecraft.Types
import Data.Maybe ( fromJust )

-- Top | Bottom | Right | Left | Front | Back
data BlockInfo = BlockInfo
  { textureIndex :: [(Int,Int)]
  , alpha :: Bool 
  , bcolor :: [(Double,Double,Double)]
  , bright :: Int
  , shape :: Shape
  }
  deriving (Eq,Show,Ord)

blockCatalog :: BlockCatalog
blockCatalog = map fst $ tail blockCatalogData

airBlockID :: BlockIDNum
airBlockID = 0
stoneBlockID :: BlockIDNum
stoneBlockID = 1
dirtBlockID :: BlockIDNum
dirtBlockID = 2
grassBlockID :: BlockIDNum
grassBlockID = 3

blockCatalogData :: [(BlockIDNum,BlockInfo)]
blockCatalogData = zip [0 .. ] 
  [ BlockInfo -- Airblockid
             { textureIndex = []
             , alpha = True
             , bcolor = replicate 6 (1.0,1.0,1.0)
             , bright = 0
             , shape = Cube
             }
  , BlockInfo -- StoneBlockID 
             { textureIndex = replicate 6 (8,2)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  , BlockInfo -- DirtBlockID 
             { textureIndex = replicate 6 (2,0)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  , BlockInfo -- GrassBlockID 
             { textureIndex = [(0,0),(2,0),(3,0),(3,0),(3,0),(3,0)]
             , alpha = False
             , bcolor = (136/255,192/255,74/255) : replicate 5 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  , BlockInfo -- GlassBlockID 
             { textureIndex = replicate 6 (1,3)
             , alpha = True
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  , BlockInfo -- WoodBlockID 
             { textureIndex = [(5,1),(5,1),(4,1),(4,1),(4,1),(4,1)]
             , alpha = False 
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  , BlockInfo -- GlowBlockID 
             { textureIndex = replicate 6 (4,13)
             , alpha = False 
             , bcolor = replicate 6 (1.0,1.0,1.0)
             , bright = 15
             , shape = Cube
             }
  , BlockInfo -- PlankBlockID 
             { textureIndex = replicate 6 (6,13)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  , BlockInfo -- PlankHalfBlockID 
             { textureIndex = replicate 6 (6,13)
             , alpha = True
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Half False
             }
  , BlockInfo --StonebrickBlockID 
             { textureIndex = replicate 6 (6,3)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  , BlockInfo -- CobbleStoneBlockID 
             { textureIndex = replicate 6 (0,1)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  , BlockInfo -- GravelBlockID 
             { textureIndex = replicate 6 (3,1)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  ,BlockInfo -- SandBlockID 
             { textureIndex = replicate 6 (2,1)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  , BlockInfo -- BrickBlockID 
             { textureIndex = replicate 6 (7,0)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  , BlockInfo -- LeavesBlockID 
             { textureIndex = replicate 6 (4,3)
             , alpha = True
             , bcolor = replicate 6 (0.2,0.7,0.2)
             , bright = 0
             , shape = Cube
             }
  , BlockInfo --RedWoolBlockID 
             { textureIndex = replicate 6 (1,8)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  , BlockInfo -- BlueWoolBlockID
             { textureIndex = replicate 6 (1,11)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  , BlockInfo -- Pumpkin
             { textureIndex = [(6,6),(6,6),(6,7),(6,7),(7,7),(6,7)]
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  , BlockInfo -- Melon 
             { textureIndex = [(9,8),(9,8),(8,8),(8,8),(8,8),(8,8)]
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  , BlockInfo -- Grass
             { textureIndex = [(12,0),(12,0)]
             , alpha = True
             , bcolor = replicate 2 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cross
             }

  ]
  -- | PressurePlates
  -- | Chest
  -- | Water
  -- | GlassPane 
  -- | Buttons
  -- | WoodenDoor

getBlockInfo :: BlockIDNum -> BlockInfo
getBlockInfo bid = fromJust $ lookup bid blockCatalogData
             
-- Top | Bottom | Right | Left | Front | Back

