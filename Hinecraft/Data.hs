{-# LANGUAGE BangPatterns #-}
--
-- Copyright : (c) T.Mishima 2014
-- License : Apache-2.0
--
module Hinecraft.Data
  ( WorldData
  , getWorldData
  , getBlockID
  , getSurfaceList
  , getAllSurfaceData
  , calcReGenArea
  , calcCursorPos
  --
  , setBlockID
  -- 
  , DataHdl
  , initData
  , exitData
  , reconfData
  , isEmpty
  , getChunkNum
  -- 
  , wIndexToChunkpos
  ) where

import qualified Data.Vector.Unboxed as DVS
import qualified Data.Vector as DVS'
import Data.Maybe ( fromJust, isJust, catMaybes ) -- , mapMaybe
import Data.List
import Data.Ord
import Data.Tuple
import Data.IORef
import qualified Data.Map as M
import System.Directory
import Control.Monad (foldM,when)
import Control.Concurrent (forkIO)
import Control.Applicative

import Hinecraft.Types
import Hinecraft.Model
import Hinecraft.Util
import Hinecraft.Rendering.Util
import Hinecraft.DB.WithSqlite3

import Debug.Trace as Dbg

data DataHdl = DataHdl
  { dbHdl :: DBHandle
  , wldDat :: IORef ( Maybe WorldData
                    , ([ChunkIdx],[(ChunkIdx,BlockNo)],[ChunkIdx]))
  , loadState :: IORef (Maybe Int)
  }

type CellChunk = [DVS.Vector BlockIDNum]
type CellChunks = M.Map (Int,Int) CellChunk
type SurfaceChunk = [M.Map Int [Surface]]
type SurfaceChunks = M.Map (Int,Int) SurfaceChunk

data WorldData = WorldData
  { cellChunks :: CellChunks
  , surfaceChunks :: SurfaceChunks
  }

-- | 

initData :: FilePath -> IO DataHdl
initData home = do
  !dbHdl' <- initDB home
  !w <- newIORef (Nothing,([],[],[]))
  !s <- newIORef Nothing
  return $! DataHdl
    { dbHdl = dbHdl'
    , wldDat = w
    , loadState = s
    }

exitData :: DataHdl -> IO ()
exitData = exitDB . dbHdl

isEmpty :: DataHdl -> IO Bool
isEmpty dhdl = do
  (t,_) <- readIORef $ wldDat dhdl 
  return $ not $ isJust t

getChunkNum :: DataHdl -> IO (Int,Int)
getChunkNum dhdl = do
  i <- readIORef $ loadState dhdl 
  return $ case i of
    Nothing -> (-1,cMax)
    Just i' -> (i',cMax)
  where cMax = length $ chunkArea (0,0,0)

getAllSurfaceData :: Maybe WorldData -> [((Int, Int), [SurfacePos])]
getAllSurfaceData wldDat = case wldDat of
  Just wld -> map (l2g wld) $ M.toList $ surfaceChunks wld 
  Nothing -> []

l2g wld ((i,k),ms) =
  ( (i,k)
  , map (\ (j,m) ->
      map (\ (p,v) ->
        let widx = chunkposToWindex ((i,k),j,p)
            Just bid = getBlockID (Just wld) widx
        in (widx, bid, v)) $ M.toList m)
      $ zip [0..] ms
  )

getWorldData :: DataHdl
             -> IO ( Maybe WorldData
                   , ([ChunkIdx],[(ChunkIdx,BlockNo)],[ChunkIdx]))
getWorldData dhdl = do
  (w,us) <- readIORef $ wldDat dhdl
  writeIORef (wldDat dhdl) (w,([],[],[])) 
  return (w,us)

getWorldData' :: DataHdl
             -> IO ( Maybe WorldData
                   , ([ChunkIdx],[(ChunkIdx,BlockNo)],[ChunkIdx]))
getWorldData' dhdl = readIORef $ wldDat dhdl


reconfData :: DataHdl -> WorldIndex -> IO ()
reconfData dhdl upos = do 
  writeIORef (loadState dhdl) $ Just 0
  forkIO $ do
    wstck <- newIORef [] :: IO (IORef [ChunkIdx])
    (wldDat',(_,u,_)) <- getWorldData' dhdl
    !wld <- do
      chLst <- mapM (\ (itr,(i,j)) -> do
        case wldDat' of
          Just wld' -> do
            ret <- case (M.lookup (i,j) $ cellChunks wld') of
              Just c' -> do
                case (M.lookup (i,j) $ surfaceChunks wld') of
                   Just f' -> return ((i,j),c',f')
                   Nothing -> return ((i,j),c',[])
              Nothing -> loadFromDB wstck itr (i,j) 
            return ret 
          Nothing -> do
            loadFromDB wstck itr (i,j) 
        ) $ zip [0..] clist 
      -- Dbg.traceIO "loadWorldData"
      return $! WorldData
        { cellChunks = M.fromList $ map (\ (i,c,_) -> (i,c)) chLst
        , surfaceChunks = M.fromList $ map (\ (i,_,f) -> (i,f)) chLst 
        }

    let (acs,dcs) = diffcs (nowclist wldDat') clist
    writeIORef (wldDat dhdl) (Just wld,(acs,u,dcs))
    writeIORef (loadState dhdl) $ Just $ length clist 

    wstck' <- readIORef wstck
    mapM_ (\ (i,j) ->
      writeChunkData dbHdl' (i,j) (vec2list gc) (map M.toList gf)) wstck'  
    return ()
  return ()
  where
    loadFromDB wstck itr (i,j) = do 
      readChunkReq dbHdl' (i,j)
      (_,cs,fs) <- readChunk dbHdl'
      chunk <- if and $ map null cs 
        then do
          modifyIORef wstck (\ s -> ((i,j):s))
          return ((i,j),gc,gf)
        else return $ genCk (i,j) cs fs
      writeIORef (loadState dhdl) $ Just itr 
      Dbg.traceIO $ unwords ["genChunk", show (i,j) , show itr]
      return chunk
    genCk (i,j) cs fs =
      ( (i,j)
      , map (\ c -> (DVS.replicate (bsize ^ 3) airBlockID) DVS.// c) cs
      , map M.fromList fs) 
    (gc,gf) = genChunk
    dbHdl' = dbHdl dhdl
    clist = chunkArea upos
    nowclist w = map (\ (i,_) -> i)
                $ case w of
                   Just cs -> M.toList $ cellChunks cs
                   Nothing -> []
    diffcs oldcs newcs = foldr (\ c (acs,oldcs') -> if elem c oldcs'
                           then (acs,filter (c /=) oldcs') 
                           else ((c:acs),oldcs')) ([],oldcs) newcs

    !bkNo = blockNum chunkParam - 1
    !bsize = blockSize chunkParam
    vec2list :: [DVS.Vector Int] -> [[(Int,BlockIDNum)]]
    vec2list = map ((filter ((/= airBlockID).snd))
                                      . ((zip [0..]).DVS.toList)) 

gIdx2lIdx :: WorldIndex -> Int
gIdx2lIdx (x,y,z) = (bsize ^ (2::Int)) * ly + bsize * lz + lx
  where
    (lx,ly,lz) = (x - ox * bsize,y - bsize * div y bsize,z - oz * bsize)
    (ox,oz) = ( x `div` bsize, z `div` bsize) 
    bsize = blockSize chunkParam

calcCursorPos :: Maybe WorldData ->  UserStatus
              -> Maybe (WorldIndex,Surface)
calcCursorPos wldDat usr = calcCursorPos' suf usr
  where
    suf = case wldDat of
            Just wld -> surfaceChunks wld
            Nothing -> M.empty

calcCursorPos' :: SurfaceChunks -> UserStatus
              -> Maybe (WorldIndex,Surface)
calcCursorPos' sufList usr = if null res
    then Nothing
    else Just $ (\ (p,(_,s)) -> (p,s))
                  $ minimumBy (comparing (\ (_,(t,_)) -> t))  $ 
                       map (\ (p,a) -> (p,fromJust a)) res
  where
    !bsize = blockSize chunkParam
    !(bx,bz) = ( round' ux `div` bsize , round' uz `div` bsize)
    !slst = M.toList $ M.filterWithKey
              (\ (i,j) v -> (bx + 1) >= i && i >= (bx - 1)
                         && (bz + 1) >= j && j >= (bz - 1)) sufList
    !f = map (\ (key,v) -> (key, filter (\ (i,_) -> elem i bplst)
                                      $ zip [0..] v)) slst
    !tf = concatMap (\ (key,m) -> map (\ (j,v) -> (key,j, M.toList v)) m) f 
    !f' = concatMap
           (\ (cidx,j,v) -> map (\ (p,bid) ->
                               (chunkposToWindex (cidx,j,p),bid)) v ) tf 
    !f'' = filter chkArea f'
    !res = filter chkJustAndFront
            $ map (tomasChk pos rot) f''
    chkArea ((sx,sy,sz),_) = dl < 8 && abs dRotY < 60
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

getSurfaceList :: Maybe WorldData -> ((Int,Int),Int) -> SurfacePos
getSurfaceList wldDat pos = case wldDat of
                Just wld -> tr $ getSurfaceList' (surfaceChunks wld) pos 
                Nothing -> [] 
  where
    !tr = map (\ (i,f) -> (i,fromJust $ getBlockID wldDat i,f))

getSurfaceList' :: SurfaceChunks -> ((Int,Int),Int)
                -> [(WorldIndex,[Surface])]
getSurfaceList' sufList (cidx,bNo) = case flst of
                                       Just flst' -> flst'
                                       Nothing -> []
  where
    !flst = (map (\ (pos,fs) -> (chunkposToWindex (cidx,bNo,pos),fs)))
            <$> M.toList <$> (\ blst -> blst !! bNo) 
            <$> (M.lookup cidx sufList) 

genSurface' :: CellChunks -> ((Int,Int),Int) -> [(WorldIndex,[Surface])]
genSurface' cchk ((i,j),bkNo) = M.toList mlst 
  where
    Just c' = (\ c -> c !! bkNo) <$> (M.lookup (i,j) cchk)
    clbk (x,y,z) = case getBlockID' cchk (x + ox, y + oy, z + oz) of
                     Just v -> v == airBlockID || alpha (getBlockInfo v)
                     Nothing -> False
    !(ox,oy,oz) = (i * bsize, bkNo * bsize, j * bsize)
    !flst = chkSuf' c' clbk
    fm m k v = M.insertWith (\ fs ofs -> fs ++ ofs) k v m 
    !mlst = foldr (\ ((x,y,z),f) m
                  -> fm m (x + ox,y + oy,z + oz) [f]) M.empty flst
    !bsize = blockSize chunkParam

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
        -> (WorldIndex -> Bool) -> [((Int, Int, Int),Surface)]
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
     -> [((Int, Int, Int), Surface)]
chkSuf'' vec (tag1,tag2) (itr1,itr2) clbk (x,y,z) = concat 
  $ case ownfill of 
      (_,True) -> [ case fill1' of
                       Just (v,False) -> [(itr1 (x,y,z),tag2)]
                       Just (_,True) -> []
                       Nothing -> []
                  , case fill2' of
                       Just (v,False) -> [(itr2 (x,y,z),tag1)]
                       Just (_,True) -> []
                       Nothing -> [] 
                  ]
      (v,False) -> [ case fill1 of
                       Just (_,True) -> [((x,y,z),tag1)]
                       Just (v',False) -> if alpha (getBlockInfo v)
                                           then [((x,y,z),tag1)
                                                ,(itr1 (x,y,z),tag2)]
                                           else []
                       Nothing -> if clbk (itr1 (x,y,z))
                          then [((x,y,z),tag1)]
                          else []
                   , case fill2 of
                       Just (_,True) -> [((x,y,z),tag2)]
                       Just (v',False) -> if alpha (getBlockInfo v)
                                           then [((x,y,z),tag1)
                                                ,(itr2 (x,y,z),tag1)]
                                           else []
                       Nothing -> if clbk (itr2 (x,y,z))
                         then [((x,y,z),tag2)]
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

chunkArea :: WorldIndex -> [(Int,Int)]
chunkArea upos = [ (i + x,j + z) | x <- [-2,-1..3], z <- [-2,-1..3] ]
-- chunkArea upos = [ (i + x,j + z) | x <- [-1,0..1], z <- [-1,0..1] ]
  where        
    ((i,j),_,_) = wIndexToChunkpos upos
        
setBlockID :: DataHdl -> WorldIndex -> BlockIDNum -> IO ()
setBlockID dtHdl pos bid = do
  (w,(a,u,d)) <- getWorldData' dtHdl
  if bid == airBlockID 
    then delObjectAtCell (dbHdl dtHdl) cidx bidx idx 
    else setObjectAtCell (dbHdl dtHdl) cidx bidx idx bid 
  newWld <- case w of
    Nothing -> return Nothing
    Just wld -> do
      let !wld' = setBlockID' wld pos bid chnklst
      mapM_ ( \ (cidx,blkNo) -> do
        let !fs = getSurfaceList' (surfaceChunks wld') (cidx,blkNo)
            !fs' = map (\ (p,f) -> let (_,_,p') = wIndexToChunkpos p 
                                  in (p',f)) fs
        writeSurfaceBlock (dbHdl dtHdl) cidx blkNo fs') 
        chnklst
      return $ Just wld' 
  writeIORef (wldDat dtHdl) (newWld,(a,u ++ chnklst,d))
  return ()
  where
    !(cidx,bidx,idx) = wIndexToChunkpos pos
    !chnklst = calcReGenArea pos

updateSufList :: CellChunks-> SurfaceChunks -> [(ChunkIdx,Int)] 
              -> SurfaceChunks
updateSufList cchk schk chnklst = foldr (\ (i,b) sfl ->
    setSurfaceList sfl (i,b) $ genSurface' cchk (i,b)) schk chnklst
  where
    setSurfaceList sufList ((i,j),bNo) sfs = M.update fn (i,j) sufList
      where
        fn blst = Just $ (take bNo blst) ++ (sfs' : drop (bNo + 1) blst)
        !sfs' = M.fromList $
                 map (\ (widx,f) -> let (_,_,pos) = wIndexToChunkpos widx
                                    in (pos,f)) sfs 

setBlockID' :: WorldData -> WorldIndex -> BlockIDNum -> [(ChunkIdx,Int)] 
            -> WorldData
setBlockID' wld (x,y,z) bid chnklst = WorldData newCCk newSuf
  where
    !newCCk = M.update (\ c -> setBlockIDfromChunk c (x,y,z) bid) key clist
    !newSuf = updateSufList newCCk slist chnklst 
    !clist = cellChunks wld
    !slist = surfaceChunks wld
    !key = ( x `div` bsize, z `div` bsize)
    !bsize = blockSize chunkParam

setBlockIDfromChunk :: CellChunk -> WorldIndex -> BlockIDNum
                    -> Maybe CellChunk
setBlockIDfromChunk c (x,y,z) bid = Just $ foldr rep [] $ zip [0 .. ] c
  where
    rep (i,d) ds = if i == div y bsize
      then (d DVS.// [(idx,bid)]):ds
      else d:ds 
    !bsize = blockSize chunkParam
    !(lx,ly,lz) = (x - ox * bsize,y - bsize * div y bsize,z - oz * bsize)
    !(ox,oz) = ( x `div` bsize, z `div` bsize) 
    !idx = (bsize ^ (2::Int)) * ly + bsize * lz + lx

getBlockID :: Maybe WorldData -> WorldIndex -> Maybe BlockIDNum
getBlockID wldDat idx =  case wldDat of
                Just wld -> getBlockID' (cellChunks wld) idx
                Nothing -> Nothing

getBlockID' :: CellChunks -> WorldIndex -> Maybe BlockIDNum
getBlockID' chmap (x,y,z)  
  | (ymin <= y) && (y < ymax ) = case M.lookup (ox,oz) chmap of
       Just c -> (c !! oy) DVS.!? idx 
       Nothing -> Nothing
  | otherwise = Nothing
  where
    !(ymin,ymax) = (0,blockSize chunkParam * blockNum chunkParam)
    !bsize = blockSize chunkParam
    !(ox,oy,oz) = (div x bsize,div y bsize,div z bsize) 
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

genChunk :: (CellChunk, SurfaceChunk)
genChunk = ( arrb ++ (arrs'' : arrt) , mapb ++ (maps : mapt))
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
    !mapb = replicate 3 ( M.fromList []) :: [M.Map Int [Surface]]
    !mapt = replicate 4 ( M.fromList []) :: [M.Map Int [Surface]]
    !maps = M.fromList $ map (\ p -> (p,[STop]))
                  [ (bsize * bsize * 2) .. (bsize * bsize * 3 - 1)]

getChunk' :: M.Map (Int,Int) CellChunk -> (Int,Int) -> Maybe CellChunk
getChunk' chmap (i,j) = M.lookup (i,j) chmap

-- #######################
--
wIndexToChunkpos :: WorldIndex -> (ChunkIdx,Int,Int)
wIndexToChunkpos (x,y,z) = ( (ox,oz) , oy, idx)
  where
    !bsize = blockSize chunkParam
    !(ox, lx) = x `divMod` bsize 
    !(oy, ly) = y `divMod` bsize
    !(oz, lz) = z `divMod` bsize 
    !idx = bsize * bsize * ly + bsize * lz + lx

chunkposToWindex :: (ChunkIdx,Int,Int) -> WorldIndex
chunkposToWindex ((i,k),j,idx) = (x,y,z)
  where
    !bsize = blockSize chunkParam
    !x = bsize * i + lx
    !y = bsize * j + ly
    !z = bsize * k + lz
    !(ly,t) = idx `divMod` (bsize * bsize)
    !(lz,lx) = t `divMod` bsize
