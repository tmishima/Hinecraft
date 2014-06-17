{-# LANGUAGE BangPatterns #-}
--
-- Copyright : (c) T.Mishima 2014
-- License : Apache-2.0
--
module Hinecraft.Data
  ( WorldData (..)
  , SurfaceList
  , SurfacePos
  , getSurface
  -- , genSurfaceList
  , setSurfaceList
  , getSurfaceList
  , getBlockID
  , setBlockID
  , calcReGenArea
  , calcCursorPos
  -- 
  , DataHdl
  , initData
  , exitData
  , loadData
  , isEmpty
  , getAllSurfaceData
  ) where

import qualified Data.Vector.Unboxed as DVS
import qualified Data.Vector as DVS'
import Control.Applicative
import Data.Maybe ( fromJust, isJust, catMaybes ) -- , mapMaybe
import Data.List
import Data.Ord
import Data.Tuple
--import Data.IORef
import qualified Data.Map as M
import System.Directory

import Hinecraft.Types
import Hinecraft.Model
import Hinecraft.Util
import Hinecraft.Render.Util
import Hinecraft.DB.WithSqlite3

import Debug.Trace as Dbg

data DataHdl = DataHdl
  { dbHdl :: DBHandle
  , wldDat :: Maybe (WorldData, SurfaceList)
  }

type SurfaceList = M.Map (Int,Int) [SurfacePos]

data WorldData = WorldData
  { chunkList :: M.Map (Int,Int) Chunk
  }

type Chunk = [DVS.Vector BlockIDNum]

data CellInfo = CellInfo
  { blockID :: BlockIDNum
  , cellType :: Int
  , baseFace :: Surface
  }
  deriving (Show,Eq,Ord)

type ChunkN = [DVS'.Vector CellInfo]

test = DVS'.replicate 3 $ CellInfo 0 0 SFront


-- | 

{-
data Te = Te { a :: [Int], b :: Double}
  deriving (Show,Eq)

test2 :: DVS'.Vector (Int,Te)
test2 = DVS'.replicate 3 (0,Te [] 4.0)
-}

initData :: FilePath -> IO DataHdl
initData home = do
  !dbHdl' <- initDB home
  return $! DataHdl
    { dbHdl = dbHdl'
    , wldDat = Nothing
     }

exitData :: DataHdl -> IO ()
exitData = exitDB . dbHdl

loadData :: DataHdl -> IO DataHdl
loadData dhdl = do
  !wld <- loadWorldData dbHdl'
  !sfl <- loadSurfaceList wld
  return $! DataHdl
    { dbHdl = dbHdl'
    , wldDat = Just (wld,sfl)
     }
  where
    dbHdl' = dbHdl dhdl

isEmpty :: DataHdl -> Bool
isEmpty dhdl = isJust $ wldDat dhdl

getAllSurfaceData :: DataHdl -> [((Int, Int), [SurfacePos])]
getAllSurfaceData dhdl = case wldDat dhdl of
  Just (_,suf) -> M.toList suf  
  Nothing -> []

loadWorldData :: DBHandle -> IO WorldData
loadWorldData dbHdl' = do
  chLst <- mapM (\ (i,j) -> do
    cs <- readChunkData dbHdl' (i,j)
    chunk <- if and $ map null cs 
      then do
        let c = genChunk
        writeChunkData dbHdl' (i,j) $ vec2list (i,j) c
        Dbg.traceIO $ "genChunk " ++ show (i,j)
        return c
      else return $ map (\ c -> (DVS.replicate (16 ^ 3) airBlockID) DVS.//
                             (map (\ (i',v) -> (gIdx2lIdx i',v)) c)
                    ) cs
    return ((i,j), chunk) 
    ) chunkArea
  Dbg.traceIO "loadWorldData"
  return $! WorldData { chunkList = M.fromList chLst }
  where
    !bkNo = blockNum chunkParam - 1


vec2list :: (Int,Int) -> [DVS.Vector Int] -> [[(WorldIndex,Int)]]
vec2list (i,j) cs = map v2l $ zip [0 .. ] cs
  where
    bsize = blockSize chunkParam
    (ox,oz) = (i * bsize, j * bsize)
    v2l (bid, c) = snd $ foldl fn (0,[]) (DVS.toList c)
      where
        fn (idx,vs) v = if v == airBlockID 
          then (idx + 1, vs)
          else (idx + 1, ((i2pos (ox,bid * bsize,oz) idx,v):vs))

loadSurfaceList :: WorldData -> IO SurfaceList
loadSurfaceList wld = do
  suLst <- mapM (\ (i,j) -> do
    return ( (i,j),
                 let c = case getChunk' (chunkList wld) (i,j) of
                           Just c' -> c'
                           Nothing -> Dbg.trace "genChunk with surface"
                                      genChunk
                 in
                 map (\ bn -> getSurface' wld c ((i,j),bn)) [0 .. bkNo]
           )
    ) chunkArea
  Dbg.traceIO "loadSurfaceList"
  return $! M.fromList suLst
  where
    !bkNo = blockNum chunkParam - 1

gIdx2lIdx :: WorldIndex -> Int
gIdx2lIdx (x,y,z) = (bsize ^ (2::Int)) * ly + bsize * lz + lx
  where
    (lx,ly,lz) = (x - ox * bsize,y - bsize * div y bsize,z - oz * bsize)
    (ox,oz) = ( x `div` bsize, z `div` bsize) 
    bsize = blockSize chunkParam

calcCursorPos :: DataHdl ->  UserStatus
              -> Maybe (WorldIndex,Surface)
calcCursorPos dtHdl usr = calcCursorPos' suf usr
  where
    suf = case wldDat dtHdl of
            Just (_,s) -> s
            Nothing -> M.empty

calcCursorPos' :: SurfaceList -> UserStatus
              -> Maybe (WorldIndex,Surface)
calcCursorPos' sufList usr = if null res
    then Nothing
    else Just $ (\ (p,(_,s)) -> (p,s))
                  $ minimumBy (comparing (\ (_,(t,_)) -> t))  $ 
                       map (\ (p,a) -> (p,fromJust a)) res
  where
    !(bx,bz) = ( round' ux `div` 16 , round' uz `div` 16)
    !slst = map (\ c' -> M.lookup c' sufList)
             [(bx + x, bz + z) | x <-[-1,0,1], z <- [-1,0,1]]
    !f = concatMap (\ s -> map (s !!) bplst) $ catMaybes slst 
    !f'' = filter chkArea (concat f)
    !res = filter chkJustAndFront
            $ map (tomasChk pos rot . (\ (a,_,b) -> (a,b))) f''
    chkArea ((sx,sy,sz),_,_) = dl < 8 && abs dRotY < 60
      where
        (_,uRotY,_) = userRot usr
        (dsx,dsy,dsz) = ( fromIntegral sx - ux
                        , fromIntegral sy - uy
                        , fromIntegral sz - uz
                        )
        dl = sqrt ( dsx ^ (2::Int) + dsy ^ (2::Int) + dsz ^ (2::Int))
        dl' = sqrt ( dsx ^ (2::Int) + dsz ^ (2::Int))
        (ex,ez) = ( cos $ pi * (uRotY - 270) / 180.0
                  , sin $ pi * (uRotY - 90) / 180.0)
        dRotY = (180 / pi) * (acos $ (ex * dsx + ez * dsz) / dl')

    chkJustAndFront (_,v) = case v of
                              Just (d,_) -> d > 0
                              Nothing -> False
    !cblkNo = div (round' uy) (blockSize chunkParam)
    bplst | cblkNo == 0 = [cblkNo, cblkNo + 1]
          | cblkNo > (blockNum chunkParam) - 2 = [cblkNo - 1, cblkNo]
          | otherwise = [cblkNo - 1, cblkNo, cblkNo + 1]
    (ux,uy,uz) = userPos usr
    !rot = (\ (a,b,c) -> (realToFrac a, realToFrac b, realToFrac c)) $ userRot usr
    !pos = (\ (a,b,c) -> (realToFrac a, realToFrac b + 1.5, realToFrac c)) $ userPos usr

tomasChk :: Pos' -> Rot' -> (WorldIndex,[Surface])
         -> (WorldIndex,Maybe (Double,Surface)) 
tomasChk pos@(px,py,pz) rot (ep,fs) = (ep, choise faceList)
  where
    !dir =(\ (x,y,z) -> (x - px, y - py, z - pz))
          $ calcPointer pos rot 1
    choise lst = if null lst 
                   then Nothing
                   else Just $ (\ (Just a,s) -> (a,s))
                                     (minimum $ map swap lst)
    !faceList = filter (\ (_,v) -> isJust v) $ zip fs $
                    map (chk . genNodeList (i2d ep)) fs
    chk ftri = if null l then Nothing else minimum l
      where !l = filter isJust $ map (\ (n1,n2,n3) ->
                           tomasMollerRaw pos dir n1 n2 n3) ftri
    genNodeList pos' face = genTri $ map ((pos' .+. )
      . (\ (a,b,c) -> (realToFrac a, realToFrac b, realToFrac c)))
      $ fst $ getVertexList Cube face
    genTri [a1,a2,a3,a4] = [(a1,a2,a3),(a3,a4,a1)]
    i2d (a,b,c) = (fromIntegral a, fromIntegral b, fromIntegral c)

calcPointer :: (Num a,Floating a) => (a,a,a) -> (a,a,a) -> a
            -> (a,a,a)
calcPointer (x,y,z) (rx,ry,_) r =
  ( x + r * ( -sin (d2r ry) * cos (d2r rx))
  , y + r * sin (d2r rx)
  , z + r * cos (d2r (ry + 180)) * cos (d2r rx))
  where
    d2r d = pi*d/180.0

{-
genSurfaceList :: WorldData -> SurfaceList
genSurfaceList wld = M.mapWithKey f $ chunkList wld
  where
    !bkNo = blockNum chunkParam - 1
    f k c = map (\ (k',bn) -> getSurface' wld c (k',bn))
                [(k,bn) | bn <-[0 .. bkNo]]
-}

setSurfaceList :: SurfaceList -> ((Int,Int),Int)
               -> SurfacePos -> SurfaceList
setSurfaceList sufList ((i,j),bNo) sfs = M.update fn (i,j) sufList
  where
    fn blst = Just $ (take bNo blst) ++ (sfs : drop (bNo + 1) blst)

getSurfaceList :: DataHdl -> ((Int,Int),Int) -> Maybe SurfacePos
getSurfaceList dtHdl pos = getSurfaceList' suf pos
  where suf = case wldDat dtHdl of
                Just (_,s) -> s
                Nothing -> M.empty

getSurfaceList' :: SurfaceList -> ((Int,Int),Int) -> Maybe SurfacePos
getSurfaceList' sufList ((i,j),bNo) =
  (\ blst -> blst !! bNo) <$> (M.lookup (i,j) sufList) 

getSurface :: WorldData -> ((Int,Int),Int) -> Maybe SurfacePos 
getSurface wld (ij,bkNo) =
  (\ c -> getSurface' wld c (ij,bkNo)) <$> (getChunk' clist ij)
  where
    !clist = chunkList wld

--type SurfacePos = [(WorldIndex,BlockIDNum,[(Surface,Bright)])]
getSurface' :: WorldData -> Chunk -> ((Int,Int),Int) -> SurfacePos 
getSurface' wld c ((i,j),bkNo) = map (\ (p,(b,f)) -> (p,b,f))
                                   $  M.toList mlst 
  where
    !c' = c !! bkNo
    clbk (x,y,z) = case getBlockID' wld (x + ox, y + oy, z + oz) of
                     Just v -> v == airBlockID || alpha (getBlockInfo v)
                     Nothing -> False
    (ox,oy,oz) = (i * bsize, bkNo * bsize, j * bsize)
    flst = chkSuf' c' clbk
    fm m k v = M.insertWith (\ (b,fs) (_,ofs) -> (b,fs ++ ofs)) k v m 
    mlst = foldr (\ ((x,y,z),b,f) m
                  -> fm m (x + ox,y + oy,z + oz) (b,[f])) M.empty flst
    bsize = blockSize chunkParam

pos2i :: WorldIndex -> Int
pos2i (x,y,z) | x < 0 || y < 0 || z < 0 = -1
              | x > bsize || y > bsize || z > bsize = -1
              | otherwise =  y * (16 * 16) + z * 16 + x
  where
    !bsize = blockSize chunkParam - 1

i2pos :: WorldIndex -> Int -> WorldIndex
i2pos (ox,oy,oz) i = (x + ox, y + oy, z + oz)
  where
    !bsize = blockSize chunkParam
    !(y, t1) = i `divMod` (bsize * bsize)
    !(z, x ) = t1 `divMod` bsize

chkSuf' :: DVS.Vector BlockIDNum
        -> (WorldIndex -> Bool) -> [((Int, Int, Int),BlockIDNum,Surface)]
chkSuf' chunk clbk = concatMap (\ p -> concat [fb p, rl p, tb p])
                       createIdx
  where
    !fb = chkSuf'' chunk (SBack,SFront)
           (\ (x,y,z) -> (x,y,z+1),\ (x,y,z) -> (x,y,z-1)) clbk 
    !rl = chkSuf'' chunk (SRight,SLeft)
           (\ (x,y,z) -> (x+1,y,z),\ (x,y,z) -> (x-1,y,z)) clbk
    !tb = chkSuf'' chunk (STop,SBottom)
           (\ (x,y,z) -> (x,y+1,z),\ (x,y,z) -> (x,y-1,z)) clbk

createIdx :: [(Int,Int,Int)]
createIdx = evv ++ odv ++ nub (sb' ++ st' ++ sf' ++ sa' ++ sr' ++ sl')
  where
    !bsize = blockSize chunkParam - 1
    !ev = [2 , 4 .. bsize - 1]
    !od = [1 , 3 .. bsize - 1]
    !evp = [(x,z) | x <- ev, z <- ev] ++ [(x,z) | x <- od, z <- od]
    !odp = [(x,z) | x <- ev, z <- od] ++ [(x,z) | x <- od, z <- ev]
    !evv = [(x,y,z) | (x,z) <- evp , y <- ev]
    !odv = [(x,y,z) | (x,z) <- odp , y <- od]
    !sb' = [ (x,0,z) | x <- [0 .. bsize], z <- [0 .. bsize]]
    !st' = [ (x,bsize,z) | x <- [0 .. bsize], z <- [0 .. bsize]]
    !sf' = [ (x,y,bsize) | x <- [0 .. bsize], y <- [0 .. bsize]]
    !sa' = [ (x,y,0) | x <- [0 .. bsize], y <- [0 .. bsize]]
    !sr' = [ (bsize,y,z) | z <- [0 .. bsize], y <- [0 .. bsize]]
    !sl' = [ (0,y,z) | z <- [0 .. bsize], y <- [0 .. bsize]]

chkSuf'' :: DVS.Vector BlockIDNum -> (Surface, Surface)
     -> ((Int, Int, Int) -> WorldIndex, (Int, Int, Int) -> WorldIndex)
     -> (WorldIndex -> Bool) -> (Int, Int, Int)
     -> [((Int, Int, Int),BlockIDNum, Surface)]
chkSuf'' vec (tag1,tag2) (itr1,itr2) clbk (x,y,z) = concat 
  $ case ownfill of 
      (_,True) -> [ case fill1' of
                       Just (v,False) -> [(itr1 (x,y,z),v,tag2)]
                       Just (_,True) -> []
                       Nothing -> []
                  , case fill2' of
                       Just (v,False) -> [(itr2 (x,y,z),v,tag1)]
                       Just (_,True) -> []
                       Nothing -> [] 
                  ]
      (v,False) -> [ case fill1 of
                       Just (_,True) -> [((x,y,z),v,tag1)]
                       Just (v',False) -> if alpha (getBlockInfo v)
                                           then [((x,y,z),v,tag1)
                                                ,(itr1 (x,y,z),v',tag2)]
                                           else []
                       Nothing -> if clbk (itr1 (x,y,z))
                          then [((x,y,z),v,tag1)]
                          else []
                   , case fill2 of
                       Just (_,True) -> [((x,y,z),v,tag2)]
                       Just (v',False) -> if alpha (getBlockInfo v)
                                           then [((x,y,z),v,tag1)
                                                ,(itr2 (x,y,z),v',tag1)]
                                           else []
                       Nothing -> if clbk (itr2 (x,y,z))
                         then [((x,y,z),v,tag2)]
                         else []
                   ]
  where
    !ownfill = (\ v -> (v,v == airBlockID)) (vec DVS.! (pos2i (x,y,z)))
    !fill1 = (\ v -> (v,v == airBlockID || alpha (getBlockInfo v)))
               <$> (vec DVS.!? ((pos2i . itr1) (x,y,z))) 
    !fill1' = (\ v -> (v,v == airBlockID))
               <$> (vec DVS.!? ((pos2i . itr1) (x,y,z))) 
    !fill2 = (\ v -> (v,v == airBlockID || alpha (getBlockInfo v)))
               <$> (vec DVS.!? ((pos2i . itr2) (x,y,z))) 
    !fill2' = (\ v -> (v,v == airBlockID))
               <$> (vec DVS.!? ((pos2i . itr2) (x,y,z))) 

chunkArea :: [(Int,Int)]
chunkArea = [ (x,z) | x <- [-1,0,1], z <- [-1,0,1] ]
         -- [ (x,z) | x <- [-4,-3 .. 4], z <- [-4,-3 .. 4] ]
        
readSurfData :: (Int,Int) -> FilePath -> IO [SurfacePos]
readSurfData (i,j) path = do
  f <- doesDirectoryExist dpath
  if f
    then read <$> readFile (dpath ++ "/suf") 
    else return []
  where
    !dpath = path ++ "/cuk" ++ show i ++ "_" ++ show j

setBlockID :: DataHdl -> WorldIndex -> BlockIDNum -> IO DataHdl
setBlockID dtHdl pos bid = do
  if bid == airBlockID 
    then delBlockInDB (dbHdl dtHdl) pos 
    else setBlockToDB (dbHdl dtHdl) pos bid
  return $! DataHdl
    { dbHdl = dbHdl dtHdl
    , wldDat = case wldDat dtHdl of
                 Just (wld,sufList) -> Just $ update wld sufList
                 Nothing -> Nothing
    }
  where
    update wld sufList = (newWld,newSuf)
      where
        !newWld = setBlockID' wld pos bid  
        !newSuf = updateSufList newWld sufList pos

updateSufList :: WorldData -> SurfaceList -> WorldIndex -> SurfaceList
updateSufList wld sufList pos = foldr (\ (i,b) sfl
   -> setSurfaceList sfl (i,b)
     $ fromJust $ getSurface wld (i,b)) sufList clst 
  where
    !clst = calcReGenArea pos 

setBlockID' :: WorldData -> WorldIndex -> BlockIDNum -> WorldData
setBlockID' wld (x,y,z) bid = WorldData
  $ M.update (\ c -> setBlockIDfromChunk c (x,y,z) bid) key clist
  where
    !clist = chunkList wld
    !key = ( x `div` bsize, z `div` bsize)
    !bsize = blockSize chunkParam

setBlockIDfromChunk :: Chunk -> WorldIndex -> BlockIDNum -> Maybe Chunk
setBlockIDfromChunk c (x,y,z) bid = Just $ foldr rep [] $ zip [0 .. ] c
  where
    rep (i,d) ds = if i == div y bsize
      then (d DVS.// [(idx,bid)]):ds
      else d:ds 
    !bsize = blockSize chunkParam
    !(lx,ly,lz) = (x - ox * bsize,y - bsize * div y bsize,z - oz * bsize)
    !(ox,oz) = ( x `div` bsize, z `div` bsize) 
    !idx = (bsize ^ (2::Int)) * ly + bsize * lz + lx

getBlockID :: DataHdl -> WorldIndex -> Maybe BlockIDNum
getBlockID hdl idx =  case wldDat hdl of
                Just (w,_) -> getBlockID' w idx
                Nothing -> Nothing

getBlockID' :: WorldData -> WorldIndex -> Maybe BlockIDNum
getBlockID' wld (x,y,z)  
  | (ymin <= y) && (y < ymax ) = case M.lookup (ox,oz) chmap of
       Just c -> (c !! oy) DVS.!? idx 
       Nothing -> Nothing
  | otherwise = Nothing
  where
    !(ymin,ymax) = (0,blockSize chunkParam * blockNum chunkParam)
    !bsize = blockSize chunkParam
    !(ox,oy,oz) = (div x bsize,div y bsize,div z bsize) 
    !chmap = chunkList wld
    !(lx,ly,lz) = (x - ox * bsize, y - oy * bsize, z - oz * bsize)
    !idx = (bsize * bsize) * ly + bsize * lz + lx

-- |
--
calcReGenArea :: WorldIndex -> [((Int,Int),BlockNo)]
calcReGenArea (x,y,z) = if null blknos
                          then [(i,bNo) | i <- chkIdx] 
                          else (org,head blknos) : [(i,bNo) | i <- chkIdx]
  where
    !bsize = blockSize chunkParam
    !blkNum = blockNum chunkParam
    !bNo = div y bsize
    !blknos | mod y bsize == 0 && bNo > 0 = [bNo - 1]
            | mod y bsize == bsize - 1 && bNo < blkNum - 1 = [bNo + 1]
            | otherwise = []
    !org = (x `div` bsize, z `div` bsize)
    !chkIdx = nub $ map (\ (x',z') -> (x' `div` bsize, z' `div` bsize))
               [(x,z),(x+1,z),(x-1,z),(x,z+1),(x,z-1)]

genChunk :: Chunk
genChunk = arrb ++ (arrs'' : arrt)
  where
    !bsize = blockSize chunkParam
    !blength = blockSize chunkParam ^ (3::Int) 
    !arrt = replicate 4 (DVS.replicate blength airBlockID)
             :: [DVS.Vector BlockIDNum]
    !arrs = DVS.replicate blength airBlockID :: DVS.Vector BlockIDNum
    !arrs' = arrs DVS.// map (\ i -> (i,dirtBlockID))
                     [0 .. (bsize * bsize * 2 - 1)]
    !arrs'' = arrs' DVS.// map (\ i -> (i,grassBlockID)) 
                     [ (bsize * bsize * 2) .. (bsize * bsize * 3 - 1)]
    !arrb = replicate 3 (DVS.replicate blength stoneBlockID)
             :: [DVS.Vector BlockIDNum]

getChunk' :: M.Map (Int,Int) Chunk -> (Int,Int) -> Maybe Chunk
getChunk' chmap (i,j) = M.lookup (i,j) chmap

