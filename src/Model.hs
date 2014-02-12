module Model where
import Data.Maybe ( fromJust )
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
data BlockInfo = BlockInfo
  { textureIndex :: [(Int,Int)]
  , alpha :: Bool 
  , enter :: Bool
  , bcolor :: [(Double,Double,Double)]
  , bright :: Int
  }

getBlockInfo :: BlockID -> BlockInfo
getBlockInfo bid = fromJust $ lookup bid db
  where
    db = [ (voidBlockID, BlockInfo
             { textureIndex = []
             , alpha = True
             , enter = True
             , bcolor = replicate 6 (1.0,1.0,1.0)
             , bright = 0
             })
         , (stoneBlockID, BlockInfo
             { textureIndex = replicate 6 (8,2)
             , alpha = False
             , enter = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             })
         , (dirtBlockID, BlockInfo
             { textureIndex = replicate 6 (2,0)
             , alpha = False
             , enter = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             })
         , (glassBlockID, BlockInfo
             { textureIndex = replicate 6 (1,3)
             , alpha = True
             , enter = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             })
         , (woodBlockID, BlockInfo
             { textureIndex = [(5,1),(5,1),(4,1),(4,1),(4,1),(4,1)]
             , alpha = False 
             , enter = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             })
         , (grassBlockID, BlockInfo
             { textureIndex = [(0,0),(2,0),(3,0),(3,0),(3,0),(3,0)]
             , alpha = False
             , enter = False
             , bcolor = (0.1,0.5,0.1) : replicate 5 (0.5,0.5,0.5)
             , bright = 0
             })
         , (glowBlockID, BlockInfo
             { textureIndex = replicate 6 (4,13)
             , alpha = False 
             , enter = False
             , bcolor = replicate 6 (1.0,1.0,1.0)
             , bright = 15
             })
         , (plankBlockID, BlockInfo
             { textureIndex = replicate 6 (6,13)
             , alpha = False
             , enter = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             })
         , (stonebrickBlockID, BlockInfo
             { textureIndex = replicate 6 (6,3)
             , alpha = False
             , enter = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             , bright = 0
             })
        ]


