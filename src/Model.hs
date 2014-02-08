module Model where
import Data.Maybe ( fromJust )

type BlockID = Int
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


-- Top | Bottom | Right | Left | Front | Back
data BlockInfo = BlockInfo
  { textureIndex :: [(Int,Int)]
  , alpha :: Bool 
  , enter :: Bool
  , bcolor :: [(Double,Double,Double)]
  }

getBlockInfo :: BlockID -> BlockInfo
getBlockInfo bid = fromJust $ lookup bid db
  where
    db = [ (voidBlockID, BlockInfo
             { textureIndex = []
             , alpha = True
             , enter = True
             , bcolor = replicate 6 (1.0,1.0,1.0)
             })
         , (stoneBlockID, BlockInfo
             { textureIndex = [(8,2),(8,2),(8,2),(8,2),(8,2),(8,2)]
             , alpha = False
             , enter = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             })
         , (dirtBlockID, BlockInfo
             { textureIndex = [(2,0),(2,0),(2,0),(2,0),(2,0),(2,0)]
             , alpha = False
             , enter = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             })
         , (glassBlockID, BlockInfo
             { textureIndex = [(1,3),(1,3),(1,3),(1,3),(1,3),(1,3)]
             , alpha = True
             , enter = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             })
         , (woodBlockID, BlockInfo
             { textureIndex = [(5,1),(5,1),(4,1),(4,1),(4,1),(4,1)]
             , alpha = False 
             , enter = False
             , bcolor = replicate 6 (0.5,0.5,0.5)
             })
         , (grassBlockID, BlockInfo
             { textureIndex = [(0,0),(2,0),(3,0),(3,0),(3,0),(3,0)]
             , alpha = False
             , enter = False
             , bcolor = (0.1,0.5,0.1) : replicate 5 (0.5,0.5,0.5)
             })
        ]


