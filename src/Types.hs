module Types where

data BlockID = VoidBlockID | StoneBlockID | DirtBlockID
             | GlassBlockID | WoodBlockID | GrassBlockID
             | GlowBlockID | PlankBlockID | StonebrickBlockID
             | PlankHalfBlockID  
             | OutOfRange
  deriving (Eq,Show,Ord)

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
