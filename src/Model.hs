module Model where
import Types

voidBlockID :: BlockID
voidBlockID = 0 
stoneBlockID :: BlockID
stoneBlockID = 1 
dirtBlockID :: BlockID
dirtBlockID = 2 
glassBlockID :: BlockID
glassBlockID = 3 
woodBlockID :: BlockID
woodBlockID = 4 
grassBlockID :: BlockID
grassBlockID = 5 
glowBlockID :: BlockID
glowBlockID = 6 
plankBlockID :: BlockID
plankBlockID = 7 
stonebrickBlockID :: BlockID
stonebrickBlockID = 8 

-- Top | Bottom | Right | Left | Front | Back

data Shape = Cube | Half | Stairs
  deriving (Eq,Show,Ord)

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
  | bid == voidBlockID = BlockInfo
             { textureIndex = []
             , alpha = True
             , bcolor = replicate 6 (1.0,1.0,1.0)
             , bright = 0
             , shape = Cube
             }
  | bid == stoneBlockID = BlockInfo
             { textureIndex = replicate 6 (8,2)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  | bid == dirtBlockID = BlockInfo
             { textureIndex = replicate 6 (2,0)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  | bid == glassBlockID = BlockInfo
             { textureIndex = replicate 6 (1,3)
             , alpha = True
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  | bid == woodBlockID = BlockInfo
             { textureIndex = [(5,1),(5,1),(4,1),(4,1),(4,1),(4,1)]
             , alpha = False 
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  | bid == grassBlockID = BlockInfo
             { textureIndex = [(0,0),(2,0),(3,0),(3,0),(3,0),(3,0)]
             , alpha = False
             , bcolor = (0.1,0.5,0.1) : replicate 5 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  | bid == glowBlockID = BlockInfo
             { textureIndex = replicate 6 (4,13)
             , alpha = False 
             , bcolor = replicate 6 (1.0,1.0,1.0)
             , bright = 15
             , shape = Cube
             }
  | bid == plankBlockID = BlockInfo
             { textureIndex = replicate 6 (6,13)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }
  | bid == stonebrickBlockID = BlockInfo
             { textureIndex = replicate 6 (6,3)
             , alpha = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             , shape = Cube
             }



