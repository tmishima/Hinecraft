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
  , loadWorldData
  , getBlockID
  , setBlockID
  , calcReGenArea
  , saveWorldData
  , saveSurfaceList
  , loadSurfaceList
  , calcCursorPos
  ) where

import qualified Data.Vector.Unboxed as DVS
--import qualified Data.Vector as DVS'
import Control.Applicative
import Data.Maybe ( fromJust, isJust, catMaybes ) -- ,, mapMaybe
import Data.List
import Data.Ord
import Data.Tuple
import Hinecraft.Types
import Hinecraft.Model
import Hinecraft.Util
import Hinecraft.Render.Util
import Debug.Trace as Dbg
import qualified Data.Map as M
import System.Directory

type SurfaceList = M.Map (Int,Int) [SurfacePos]

data ChunkParam = ChunkParam
  { blockSize :: Int
  , blockNum  :: Int
  }

chunkParam :: ChunkParam
chunkParam = ChunkParam
  { blockSize = 16
  , blockNum = 8
  }

data WorldData = WorldData
  { chunkList :: M.Map (Int,Int) Chunk
  }
type Chunk = [DVS.Vector BlockIDNum]

-- | 

{-
data Te = Te { a :: [Int], b :: Double}
  deriving (Show,Eq)

test2 :: DVS'.Vector (Int,Te)
test2 = DVS'.replicate 3 (0,Te [] 4.0)
-}

calcCursorPos :: SurfaceList -> UserStatus
              -> Maybe (WorldIndex,Surface)
calcCursorPos sufList usr = if null res
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
    chkArea ((sx,sy,sz),_,_) = sqrt ( (fromIntegral sx - ux) ^ (2::Int)
                                  + (fromIntegral sy - uy) ^ (2::Int)
                                  + (fromIntegral sz - uz) ^ (2::Int)) < 8
    chkJustAndFront (_,v) = case v of
                              Just (d,_) -> d > 0
                              Nothing -> False
    cblkNo = div (round' uy) (blockSize chunkParam)
    bplst | cblkNo == 0 = [cblkNo, cblkNo + 1]
          | cblkNo > (blockNum chunkParam) - 2 = [cblkNo - 1, cblkNo]
          | otherwise = [cblkNo - 1, cblkNo, cblkNo + 1]
    (ux,uy,uz) = userPos usr
    rot = (\ (a,b,c) -> (realToFrac a, realToFrac b, realToFrac c)) $ userRot usr
    pos = (\ (a,b,c) -> (realToFrac a, realToFrac b + 1.5, realToFrac c)) $ userPos usr

tomasChk :: Pos' -> Rot' -> (WorldIndex,[Surface])
         -> (WorldIndex,Maybe (Double,Surface)) 
tomasChk pos@(px,py,pz) rot (ep,fs) = (ep, choise faceList)
  where
    dir =(\ (x,y,z) -> (x - px, y - py, z - pz))
          $ calcPointer pos rot 1
    choise lst = if null lst 
                   then Nothing
                   else Just $ (\ (Just a,s) -> (a,s))
                                     (minimum $ map swap lst)
    faceList = filter (\ (_,v) -> isJust v) $ zip fs $
                    map (chk . genNodeList (i2d ep)) fs
    chk ftri = if null l then Nothing else minimum l
      where !l = filter isJust $ map (\ (n1,n2,n3) ->
                           tomasMollerRaw pos dir n1 n2 n3) ftri
    genNodeList pos' face = genTri $ map ((pos' .+. )
      . (\ ((a,b,c),_) -> (realToFrac a, realToFrac b, realToFrac c)))
      $ getVertexList Cube face
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

getSurfaceList :: SurfaceList -> ((Int,Int),Int) -> Maybe SurfacePos
getSurfaceList sufList ((i,j),bNo) =
  (\ blst -> blst !! bNo) <$> (M.lookup (i,j) sufList) 

getSurface :: WorldData -> ((Int,Int),Int) -> Maybe SurfacePos 
getSurface wld (ij,bkNo) =
  (\ c -> getSurface' wld c (ij,bkNo)) <$> (getChunk' clist ij)
  where
    clist = chunkList wld

--type SurfacePos = [(WorldIndex,BlockIDNum,[(Surface,Bright)])]
getSurface' :: WorldData -> Chunk -> ((Int,Int),Int) -> SurfacePos 
getSurface' wld c ((i,j),bkNo) = map (\ (p,(b,f)) -> (p,b,f))
                                   $  M.toList mlst 
  where
    !c' = c !! bkNo
    clbk (x,y,z) = case getBlockID wld (x + ox, y + oy, z + oz) of
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
    bsize = blockSize chunkParam - 1

chkSuf' :: DVS.Vector BlockIDNum
        -> (WorldIndex -> Bool) -> [((Int, Int, Int),BlockIDNum,Surface)]
chkSuf' chunk clbk = concatMap (\ p -> concat [fb p, rl p, tb p])
                       createIdx
  where
    fb = chkSuf'' chunk (SBack,SFront)
           (\ (x,y,z) -> (x,y,z+1),\ (x,y,z) -> (x,y,z-1)) clbk 
    rl = chkSuf'' chunk (SRight,SLeft)
           (\ (x,y,z) -> (x+1,y,z),\ (x,y,z) -> (x-1,y,z)) clbk
    tb = chkSuf'' chunk (STop,SBottom)
           (\ (x,y,z) -> (x,y+1,z),\ (x,y,z) -> (x,y-1,z)) clbk

createIdx :: [(Int,Int,Int)]
createIdx = evv ++ odv ++ nub (sb' ++ st' ++ sf' ++ sa' ++ sr' ++ sl')
  where
    bsize = blockSize chunkParam - 1
    ev = [2 , 4 .. bsize - 1]
    od = [1 , 3 .. bsize - 1]
    evp = [(x,z) | x <- ev, z <- ev] ++ [(x,z) | x <- od, z <- od]
    odp = [(x,z) | x <- ev, z <- od] ++ [(x,z) | x <- od, z <- ev]
    evv = [(x,y,z) | (x,z) <- evp , y <- ev]
    odv = [(x,y,z) | (x,z) <- odp , y <- od]
    sb' = [ (x,0,z) | x <- [0 .. bsize], z <- [0 .. bsize]]
    st' = [ (x,bsize,z) | x <- [0 .. bsize], z <- [0 .. bsize]]
    sf' = [ (x,y,bsize) | x <- [0 .. bsize], y <- [0 .. bsize]]
    sa' = [ (x,y,0) | x <- [0 .. bsize], y <- [0 .. bsize]]
    sr' = [ (bsize,y,z) | z <- [0 .. bsize], y <- [0 .. bsize]]
    sl' = [ (0,y,z) | z <- [0 .. bsize], y <- [0 .. bsize]]

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
    ownfill = (\ v -> (v,v == airBlockID)) (vec DVS.! (pos2i (x,y,z)))
    fill1 = (\ v -> (v,v == airBlockID || alpha (getBlockInfo v)))
               <$> (vec DVS.!? ((pos2i . itr1) (x,y,z))) 
    fill1' = (\ v -> (v,v == airBlockID))
               <$> (vec DVS.!? ((pos2i . itr1) (x,y,z))) 
    fill2 = (\ v -> (v,v == airBlockID || alpha (getBlockInfo v)))
               <$> (vec DVS.!? ((pos2i . itr2) (x,y,z))) 
    fill2' = (\ v -> (v,v == airBlockID))
               <$> (vec DVS.!? ((pos2i . itr2) (x,y,z))) 

chunkArea :: [(Int,Int)]
chunkArea = [ (x,z) | x <- [-2,-1 .. 2], z <- [-2,-1 .. 2] ]
        --  [ (x,z) | x <- [-4,-3 .. 4], z <- [-4,-3 .. 4] ]
        
saveWorldData :: WorldData -> FilePath -> IO ()
saveWorldData wld home = do
  --tmp <- getTemporaryDirectory
  --let tpath = tmp ++ "/wld0"
  --createDirWithChk tpath 
  mapM_ (\ (k,c) -> writeChunkData k c path) chLst
  where
    chLst = M.toList $ chunkList wld
    path = home ++ "/.Hinecraft/userdata/wld0"

loadWorldData :: FilePath -> IO WorldData
loadWorldData home = do
  chLst <- mapM (\ (i,j) -> do
    c <- readChunkData (i,j) path
    return ((i,j), case c of 
                     Just c' -> c'
                     Nothing -> genChunk)
    ) chunkArea
  Dbg.traceIO "loadWorldData"
  return $! WorldData { chunkList = M.fromList chLst }
  where
    path = home ++ "/.Hinecraft/userdata/wld0"

loadSurfaceList :: WorldData -> FilePath -> IO SurfaceList
loadSurfaceList wld home = do
  suLst <- mapM (\ (i,j) -> do
    s <- readSurfData (i,j) path
    return ( (i,j)
           , if null s 
               then
                 let c = case getChunk' (chunkList wld) (i,j) of
                           Just c' -> c'
                           Nothing -> genChunk
                 in
                 map (\ bn -> getSurface' wld c ((i,j),bn)) [0 .. bkNo]
               else s
           )
    ) chunkArea
  Dbg.traceIO "loadSurfaceList"
  return $! M.fromList suLst
  where
    path = home ++ "/.Hinecraft/userdata/wld0"
    !bkNo = blockNum chunkParam - 1

readSurfData :: (Int,Int) -> FilePath -> IO [SurfacePos]
readSurfData (i,j) path = do
  f <- doesDirectoryExist dpath
  if f
    then read <$> readFile (dpath ++ "/suf") 
    else return []
  where
    dpath = path ++ "/cuk" ++ show i ++ "_" ++ show j

saveSurfaceList :: SurfaceList -> FilePath -> IO () 
saveSurfaceList suflst home =
  mapM_ (\ (k,s) -> writeSurfListData k s path) suLst
  where
    suLst = M.toList suflst 
    path = home ++ "/.Hinecraft/userdata/wld0"

writeSurfListData :: (Int,Int) -> [SurfacePos] -> FilePath -> IO ()
writeSurfListData (i,j) s path = do
  createDirWithChk dpath
  writeFile (dpath ++ "/suf") $ show s 
  where
    dpath = path ++ "/cuk" ++ show i ++ "_" ++ show j

writeChunkData :: (Int,Int) -> Chunk -> FilePath -> IO ()
writeChunkData (i,j) ch path = do
  createDirWithChk dpath
  writeFile (dpath ++ "/blk") $ show ch 
  where
    dpath = path ++ "/cuk" ++ show i ++ "_" ++ show j

readChunkData :: (Int,Int) -> FilePath -> IO (Maybe Chunk)
readChunkData (i,j) path = do
  f <- doesDirectoryExist dpath
  if f
    then do
      ch <- read <$> readFile (dpath ++ "/blk") 
      return $! Just ch 
    else return Nothing
  where
    dpath = path ++ "/cuk" ++ show i ++ "_" ++ show j

createDirWithChk :: FilePath -> IO Bool
createDirWithChk path = do
  f <- doesDirectoryExist path
  if f 
    then return False
    else createDirectory path >> return True

setBlockID :: WorldData -> WorldIndex -> BlockIDNum -> WorldData
setBlockID wld (x,y,z) bid = WorldData
  $ M.update (\ c -> setBlockIDfromChunk c (x,y,z) bid) key clist
  where
    clist = chunkList wld
    key = ( x `div` bsize, z `div` bsize)
    bsize = blockSize chunkParam

setBlockIDfromChunk :: Chunk -> WorldIndex -> BlockIDNum -> Maybe Chunk
setBlockIDfromChunk c (x,y,z) bid = Just $ foldr rep [] $ zip [0 .. ] c
  where
    rep (i,d) ds = if i == div y bsize
      then (d DVS.// [(idx,bid)]):ds
      else d:ds 
    bsize = blockSize chunkParam
    (lx,ly,lz) = (x - ox * bsize,y - bsize * div y bsize,z - oz * bsize)
    (ox,oz) = ( x `div` bsize, z `div` bsize) 
    idx = (bsize ^ (2::Int)) * ly + bsize * lz + lx

getBlockID :: WorldData -> WorldIndex -> Maybe BlockIDNum
getBlockID wld (x,y,z)  
  | (ymin <= y) && (y < ymax ) = case M.lookup (ox,oz) chmap of
       Just c -> (c !! oy) DVS.!? idx 
       Nothing -> Nothing
  | otherwise = Nothing
  where
    (ymin,ymax) = (0,blockSize chunkParam * blockNum chunkParam)
    !bsize = blockSize chunkParam
    !(ox,oy,oz) = (div x bsize,div y bsize,div z bsize) 
    !chmap = chunkList wld
    (lx,ly,lz) = (x - ox * bsize, y - oy * bsize, z - oz * bsize)
    idx = (bsize * bsize) * ly + bsize * lz + lx

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
    org = (x `div` bsize, z `div` bsize)
    chkIdx = nub $ map (\ (x',z') -> (x' `div` bsize, z' `div` bsize))
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

