module Data
  ( WorldData (..)
  , Chunk (..)
  , SurfaceList
  , SurfacePos
  , getSurface
  , genSurfaceList
  , setSurfaceList
  , getSunLightEffect
  , getCompliePosList
  , ChunkParam (..)
  , getChunk
  , chunkParam
  , genWorldData
  , getBlockID
  , setBlockID
  , calcReGenArea
  , calcSunLight
  , initSunLight
  ) where

import Data.IORef
import Data.Maybe ( fromJust , catMaybes ) --,isJust )
import Data.Array.IO
import Control.Monad ( replicateM, {- unless,when, void,filterM-} )
--import Control.Applicative
import Types
import Model

 
type SurfaceList = IORef [(ChunkNo, [(BlockNo,IORef SurfacePos)])]

data ChunkParam = ChunkParam
  { blockSize :: Int
  , blockNum  :: Int
  }

data WorldData = WorldData
  { chunkList :: IORef [(ChunkNo,Chunk)]
  }

data Chunk = Chunk
  { origin :: (Int,Int)
  , local :: [(Int,IOUArray Int Int)]
  , sunLight :: IOUArray Int Int
  }

genSurfaceList :: WorldData -> IO SurfaceList
genSurfaceList wld = readIORef chl
  >>= mapM (\ (cNo,_) -> do
    spos <- mapM (\ b -> do
      fs' <- getSurface wld (cNo,b)
      fs <- newIORef fs'
      return (b,fs)) [0 .. bkNo]
    return (cNo,spos)) 
      >>= newIORef
  where
    chl = chunkList wld 
    bkNo = blockNum chunkParam - 1

setSurfaceList :: SurfaceList -> (Int,Int) -> SurfacePos -> IO ()
setSurfaceList sufList (cNo,bNo) sfs = do
  s <- readIORef sufList
  let b = fromJust $ lookup bNo $ fromJust $ lookup cNo s 
  writeIORef b sfs 
  return ()

getSurface :: WorldData -> (ChunkNo,Int) 
           -> IO SurfacePos 
getSurface wld (chNo,bkNo) = do
  blkpos <- getCompliePosList wld (chNo,bkNo)
  blks <- mapM (getBlockID wld) blkpos
            >>= return . (filter (\ (_,bid) -> bid /= voidBlockID ))
                       . (zip blkpos)
  blks' <- mapM (\ (pos,bid) -> do
    fs <- getSuf pos >>= return . catMaybes
    return (pos,bid,fs)) blks
  return $ (filter (\ (_,_,fs) -> not $ null fs)) blks'
  where
    getAroundIndex (x',y',z') = [ (SRight,(x' + 1, y', z'))
                                , (SLeft, (x' - 1, y', z'))
                                , (STop, (x', y' + 1, z'))
                                , (SBottom, (x', y' - 1, z'))
                                , (SBack,(x', y', z' + 1))
                                , (SFront,(x', y', z' - 1))
                                ]
    getSuf (x',y',z') = mapM (\ (f,pos) -> do
        b <- getBlockID wld pos
        sun <- getSunLightEffect wld pos 
        return $ if b == voidBlockID
                   || b == (-1) || alpha (getBlockInfo b) 
           then Just (f,if sun then 16 else 5) 
           else Nothing 
        ) (getAroundIndex (x',y',z')) 

{-
getSuface :: SurfaceList -> (ChunkNo,Int) -> IO (Maybe SurfacePos)
getSuface suf (chNo,bNo) = do
  suf' <- readIORef suf
  case lookup chNo suf' of
    Just chl -> case lookup bNo chl of
      Just blk -> readIORef blk >>= return . Just
      Nothing -> return Nothing
    Nothing -> return Nothing
-}

genWorldData :: IO WorldData
genWorldData = do
  cl <- newIORef . zip [0 ..]
    =<< mapM (\ lst -> do
            c <- genChunk lst
            initSunLight c
            return c)
          [ (x,z) | x <- [-16,0 .. 16], z <- [-16,0 .. 16] ]
        --  [ (x,z) | x <- [-32,-16 .. 32], z <- [-32,-16 .. 32] ]
        --  [ (x,z) | x <- [-64,-48 .. 48], z <- [-64,-48 .. 48] ]
  return WorldData 
    { chunkList =  cl
    }

setBlockID :: WorldData -> WorldIndex -> BlockID
           -> IO ()
setBlockID wld (x,y,z) bid = do
  chl <- readIORef (chunkList wld)
  case getChunk chl (x,y,z) of
    Just (_,c) -> do
      setBlockIDfromChunk c (x,y,z) bid
      let (ox,oz) = origin c
      calcSunLight c (x - ox,z - oz)
    Nothing -> return () 

setBlockIDfromChunk :: Chunk -> WorldIndex -> BlockID -> IO ()
setBlockIDfromChunk c (x,y,z) = writeArray arr idx 
  where
    bsize = blockSize chunkParam
    (lx,ly,lz) = (x - ox, y - bsize * div y bsize, z - oz )
    (ox,oz) = origin c
    dat = local c
    arr = fromJust $ lookup (div y bsize) dat
    idx = (bsize ^ (2::Int)) * ly + bsize * lz + lx

getBlockID :: WorldData -> WorldIndex -> IO BlockID
getBlockID wld (x,y,z) = do
  chl <- readIORef chlist
  case getChunk chl (x,y,z) of
    Just (_,c) -> getBlockIDfromChunk c (x,y,z)
    Nothing -> return (-1)
  where
    chlist = chunkList wld

getBlockIDfromChunk :: Chunk -> WorldIndex -> IO BlockID
getBlockIDfromChunk c (x,y,z) = readArray arr idx
  where
    bsize = blockSize chunkParam
    (lx,ly,lz) = (x - ox, y - (div y bsize) * bsize, z - oz)
    (ox,oz) = origin c
    dat = local c
    arr = fromJust $ lookup (div y bsize) dat
    idx = (bsize * bsize) * ly + bsize * lz + lx

chunkParam :: ChunkParam
chunkParam = ChunkParam
  { blockSize = 16
  , blockNum = 8
  }

calcReGenArea :: WorldData -> WorldIndex -> IO [(ChunkNo,BlockNo)]
calcReGenArea wld (x,y,z) = do
  chl <- readIORef chlist
  case getChunk chl (x,y,z) of
    Nothing -> return [] 
    Just (cNo,_) -> return $
         map (\ bno' -> (cNo,bno')) blknos 
      ++ map (\ cno' -> (cno',bNo)) (cNoX chl)
      ++ map (\ cno' -> (cno',bNo)) (cNoZ chl)
  where
    bsize = blockSize chunkParam
    blkNum = blockNum chunkParam
    chlist = chunkList wld
    bNo = div y bsize
    blknos | mod y bsize == 0 && bNo > 0 = [bNo,bNo - 1]
           | mod y bsize == bsize - 1 && bNo < blkNum - 1 = [bNo,bNo + 1]
           | otherwise = [bNo]
    cNoX chl | mod x bsize == 0 = case getChunk chl (x - 1,y,z) of
                                   Nothing -> [] 
                                   Just (cNo,_) -> [cNo]
             | mod x bsize == bsize - 1 = case getChunk chl (x + 1,y,z) of
                                   Nothing -> [] 
                                   Just (cNo,_) -> [cNo]
             | otherwise = []
    cNoZ chl | mod z bsize == 0 = case getChunk chl (x,y,z - 1) of
                                   Nothing -> [] 
                                   Just (cNo,_) -> [cNo]
             | mod z bsize == bsize - 1 = case getChunk chl (x,y,z + 1) of
                                   Nothing -> [] 
                                   Just (cNo,_) -> [cNo]
             | otherwise = []

genChunk :: (Int,Int) -> IO Chunk
genChunk org = do
  arrt <- replicateM 4
    (newArray (0, blength) voidBlockID) :: IO [IOUArray Int Int]

  arrs <- newArray (0,blength) voidBlockID :: IO (IOUArray Int Int)
  mapM_ (\ i -> writeArray arrs i dirtBlockID) [0 .. 16 * 16 * 2 - 1]
  mapM_ (\ i -> writeArray arrs i grassBlockID)
           [ 16 * 16 * 2 .. 16 * 16 * 3 - 1]

  arrb <- replicateM 3
    (newArray (0,blength) stoneBlockID) :: IO [IOUArray Int Int]

  sun' <- newArray (0, ((blockSize chunkParam) ^ (2::Int)) - 1) 0

  return Chunk
    { origin = org
    , local = zip [0 .. ] $ arrb ++ (arrs : arrt)
    , sunLight = sun'
    } 
  where
    blength = (blockSize chunkParam) ^ (3::Int) -1

getChunk :: [(ChunkNo,Chunk)] -> WorldIndex -> Maybe (ChunkNo,Chunk)
getChunk [] _ = Nothing 
getChunk (c:cs) (x,y,z) | (ox <= x) && (x < ox + bsize) &&
                          (oz <= z) && (z < oz + bsize) &&
                          (ymin <= y) && (y < ymax )
                           = Just c  
                        | otherwise = getChunk cs (x,y,z)
  where
    (ox,oz) = origin $ snd c
    (ymin,ymax) = (0,blockSize chunkParam * blockNum chunkParam)
    bsize = blockSize chunkParam

getCompliePosList :: WorldData -> (ChunkNo,Int) -> IO [WorldIndex]
getCompliePosList wld (chNo,blkNo) = do
  chunk <- fmap (fromJust . (lookup chNo)) (readIORef $ chunkList wld)
  let (x',z') = origin chunk 
      y' = blkNo * bsize
      (sx,sy,sz) = (f x', f y', f z')
  return [(x,y,z) | x <- [sx .. sx + bsize - 1]
                  , y <- [sy .. sy + bsize - 1]
                  , z <- [sz .. sz + bsize - 1]]
  where
    bsize = blockSize chunkParam
    f a = bsize * (div a bsize)
 

initSunLight :: Chunk -> IO ()
initSunLight chunk = mapM_ (calcSunLight chunk) 
      [(x,z) | x <- lst, z <- lst]
  where
    lst = [0 .. blockSize chunkParam - 1]

calcSunLight :: Chunk -> (Int,Int) -> IO ()
calcSunLight chunk pos =
  chk pos blks initY
    >>= writeArray (sunLight chunk) (calcIdx pos)
  where 
    blks = reverse $ local chunk
    initY = blockNum chunkParam * blockSize chunkParam - 1
    calcIdx (x,z) = z * blockSize chunkParam + x
    chk _ [] y = return y
    chk (x,z) (b:bs) y = do
      v <- chk' (snd b) (calcIdx (x,z)) $ blockSize chunkParam - 1
      if v < 0
        then chk (x,z) bs (y - blockSize chunkParam)
        else return (y - blockSize chunkParam + v + 1)
                                              -- +1 して Indexを数へ変換
    layerSize = blockSize chunkParam ^ (2::Int)
    chk' :: IOUArray Int Int -> Int -> Int -> IO Int
    chk' blk offset count
      | count < 0 = return (-1)
      | otherwise = do
          v <- readArray blk $ count * layerSize + offset
          if v == voidBlockID || alpha (getBlockInfo v)
            then chk' blk offset (count - 1)
            else return (count + 1) -- 一つ前のIndexへ戻す

getSunLightEffect :: WorldData -> WorldIndex -> IO Bool
getSunLightEffect wld pos@(x,y,z) = do
  chl <- readIORef chlist
  case getChunk chl pos of
    Just (_,cnk) -> do
      let (ox,oz) = origin cnk
      readArray (sunLight cnk) (calcIdx (x - ox,z - oz))
        >>= return . (y >=)  
    Nothing -> return False
  where
    chlist = chunkList wld
    calcIdx (x',z') = z' * (blockSize chunkParam) + x'


