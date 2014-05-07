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
  , isCollision :: Bool
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
             , isCollision = True
             }
  , BlockInfo -- StoneBlockID 
             { textureIndex = replicate 6 (8,2)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             , isCollision = True
             }
  , BlockInfo -- DirtBlockID 
             { textureIndex = replicate 6 (2,0)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             , isCollision = True
             }
  , BlockInfo -- GrassBlockID 
             { textureIndex = [(0,0),(2,0),(3,0),(3,0),(3,0),(3,0)]
             , alpha = False
             , bcolor = (136/255,192/255,74/255) : replicate 5 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             , isCollision = True
             }
  , BlockInfo -- GlassBlockID 
             { textureIndex = replicate 6 (1,3)
             , alpha = True
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             , isCollision = True
             }
  , BlockInfo -- WoodBlockID 
             { textureIndex = [(5,1),(5,1),(4,1),(4,1),(4,1),(4,1)]
             , alpha = False 
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             , isCollision = True
             }
  , BlockInfo -- GlowBlockID 
             { textureIndex = replicate 6 (4,13)
             , alpha = False 
             , bcolor = replicate 6 (1.0,1.0,1.0)
             , bright = 15
             , shape = Cube
             , isCollision = True
             }
  , BlockInfo -- PlankBlockID 
             { textureIndex = replicate 6 (6,13)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             , isCollision = True
             }
  , BlockInfo -- PlankHalfBlockID 
             { textureIndex = replicate 6 (6,13)
             , alpha = True
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Half False
             , isCollision = True
             }
  , BlockInfo --StonebrickBlockID 
             { textureIndex = replicate 6 (6,3)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             , isCollision = True
             }
  , BlockInfo -- CobbleStoneBlockID 
             { textureIndex = replicate 6 (0,1)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             , isCollision = True
             }
  , BlockInfo -- GravelBlockID 
             { textureIndex = replicate 6 (3,1)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             , isCollision = True
             }
  ,BlockInfo -- SandBlockID 
             { textureIndex = replicate 6 (2,1)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             , isCollision = True
             }
  , BlockInfo -- BrickBlockID 
             { textureIndex = replicate 6 (7,0)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             , isCollision = True
             }
  , BlockInfo -- LeavesBlockID 
             { textureIndex = replicate 6 (4,3)
             , alpha = True
             , bcolor = replicate 6 (0.2,0.7,0.2)
             , bright = 0
             , shape = Cube
             , isCollision = True
             }
  , BlockInfo --RedWoolBlockID 
             { textureIndex = replicate 6 (1,8)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             , isCollision = True
             }
  , BlockInfo -- BlueWoolBlockID
             { textureIndex = replicate 6 (1,11)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             , isCollision = True
             }
  , BlockInfo -- Pumpkin
             { textureIndex = [(6,6),(6,6),(6,7),(6,7),(7,7),(6,7)]
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             , isCollision = True
             }
  , BlockInfo -- Melon 
             { textureIndex = [(9,8),(9,8),(8,8),(8,8),(8,8),(8,8)]
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             , isCollision = True
             }
  , BlockInfo -- Flower1 
             { textureIndex = [(12,0)]
             , alpha = True
             , bcolor = [(0.5,0.5,0.5)]
             , bright = 0
             , shape = Cross
             , isCollision = False
             }
  , BlockInfo -- Flower2 
             { textureIndex = [(13,0)]
             , alpha = True
             , bcolor = [(0.5,0.5,0.5)]
             , bright = 0
             , shape = Cross
             , isCollision = False
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

