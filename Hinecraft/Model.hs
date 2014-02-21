module Hinecraft.Model where
import Hinecraft.Types

-- Top | Bottom | Right | Left | Front | Back
data BlockInfo = BlockInfo
  { textureIndex :: [(Int,Int)]
  , alpha :: Bool 
  , bcolor :: [(Double,Double,Double)]
  , bright :: Int
  , shape :: Shape
  }
  deriving (Eq,Show,Ord)

getBlockInfo :: BlockID -> BlockInfo
getBlockInfo bid
  | bid == AirBlockID = BlockInfo
             { textureIndex = []
             , alpha = True
             , bcolor = replicate 6 (1.0,1.0,1.0)
             , bright = 0
             , shape = Cube
             }
  | bid == StoneBlockID = BlockInfo
             { textureIndex = replicate 6 (8,2)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  | bid == DirtBlockID = BlockInfo
             { textureIndex = replicate 6 (2,0)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  | bid == GlassBlockID = BlockInfo
             { textureIndex = replicate 6 (1,3)
             , alpha = True
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  | bid == WoodBlockID = BlockInfo
             { textureIndex = [(5,1),(5,1),(4,1),(4,1),(4,1),(4,1)]
             , alpha = False 
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  | bid == GrassBlockID = BlockInfo
             { textureIndex = [(0,0),(2,0),(3,0),(3,0),(3,0),(3,0)]
             , alpha = False
             , bcolor = (0.1,0.5,0.1) : replicate 5 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  | bid == GlowBlockID = BlockInfo
             { textureIndex = replicate 6 (4,13)
             , alpha = False 
             , bcolor = replicate 6 (1.0,1.0,1.0)
             , bright = 15
             , shape = Cube
             }
  | bid == PlankBlockID = BlockInfo
             { textureIndex = replicate 6 (6,13)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  | bid == PlankHalfBlockID = BlockInfo
             { textureIndex = replicate 6 (6,13)
             , alpha = True
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Half False
             }
  | bid == StonebrickBlockID = BlockInfo
             { textureIndex = replicate 6 (6,3)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  | bid == CobbleStoneBlockID = BlockInfo
             { textureIndex = replicate 6 (0,1)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  | bid == GravelBlockID = BlockInfo
             { textureIndex = replicate 6 (3,1)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  | bid == SandBlockID = BlockInfo
             { textureIndex = replicate 6 (2,1)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  | bid == BrickBlockID = BlockInfo
             { textureIndex = replicate 6 (7,0)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  | bid == LeavesBlockID = BlockInfo
             { textureIndex = replicate 6 (4,3)
             , alpha = True
             , bcolor = replicate 6 (0.2,0.7,0.2)
             , bright = 0
             , shape = Cube
             }
  | bid == RedWoolBlockID = BlockInfo
             { textureIndex = replicate 6 (1,8)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  | bid == BlueWoolBlockID = BlockInfo
             { textureIndex = replicate 6 (1,11)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }

