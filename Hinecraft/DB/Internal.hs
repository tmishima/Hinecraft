{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Hinecraft.DB.Internal where

import qualified Data.Text as T
import Control.Applicative
import Database.SQLite.Simple

import Hinecraft.Types

type ChunkIdx = (Int,Int)
type ChunkID = Int
type BlockID = Int
type Index = Int

-- ######### Chunk Table ##########

createChunkTableSQL :: T.Text
createChunkTableSQL =
  "CREATE TABLE ChunkTable ( \
  \ i_index INTEGER NOT NULL, \
  \ j_index INTEGER NOT NULL, \
  \ ChunkID INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL )"

data ChunkIDField = ChunkIDField Int deriving (Show)

instance FromRow ChunkIDField where
  fromRow = ChunkIDField <$> field

addChunk :: Connection -> ChunkIdx -> IO ()
addChunk conn (i,j) = execute conn sql (i,j) 
  where
    sql = "INSERT INTO ChunkTable \
          \ ( i_index , j_index ) \
          \ values \
          \ (?, ?)"

checkChunk :: Connection -> ChunkIdx -> IO Bool
checkChunk conn (i,j) = do
  r <- query conn sql (i,j) :: IO [ChunkIDField]
  return $ case r of
    [] -> False 
    _ -> True
  where
    sql = " Select ChunkID from ChunkTable \
          \   where i_index = ? \
          \     and j_index = ? "

_getChunkID :: Connection -> ChunkIdx -> IO (Maybe Int)
_getChunkID conn (i,j) = do
  r <- query conn sql (i,j) :: IO [ChunkIDField]
  return $ case r of
    [] -> Nothing
    ((ChunkIDField chi):_) -> Just chi 
  where
    sql = " Select ChunkID from ChunkTable \
          \   where i_index = ? \
          \     and j_index = ? "

-- ######### Block-Chunk Table ##########

createBlockChunkTableSQL :: T.Text
createBlockChunkTableSQL =
  "CREATE TABLE BlockChunkTable ( \
  \ BlockID INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, \ 
  \ ChunkID INTEGER NOT NULL, \ 
  \ HPos    INTEGER NOT NULL )" 

data BlockChunkField = BlockChunkField Int deriving (Show)

instance FromRow BlockChunkField where
  fromRow = BlockChunkField <$> field

addBlockToChunk :: Connection -> (ChunkIdx,Index) -> IO ()
addBlockToChunk conn ((i,j),pos) = execute conn sql (i,j,pos)
  where
    sql = "INSERT INTO BlockChunkTable ( ChunkID, HPos ) \
          \ values \
          \ ( \
          \   (Select ChunkID from chunktable \
          \      where i_index = ? and j_index = ? ) \
          \   , ? ) "

_getBlockID :: Connection -> ChunkIdx -> Index -> IO (Maybe Int)
_getBlockID conn (i,j) hpos = do
  r <- query conn sql (i,j,hpos) :: IO [BlockChunkField]
  return $ case r of
    [] -> Nothing
    ((BlockChunkField blki):_) -> Just blki 
  where
    sql = "Select BlockID from BlockChunkTable \
          \   where ChunkID = (Select ChunkID from chunktable \
          \                      where i_index = ? and j_index = ?) \
          \         and HPos = ? "

-- ######### Block-Obj Table ##########

createBlockObjTableSQL :: T.Text
createBlockObjTableSQL =
  "CREATE TABLE BlockObjTable ( \
  \ BlockID INTEGER NOT NULL, \
  \ pos INTEGER NOT NULL, \
  \ ObjectID INTEGER NOT NULL, \
  \ PRIMARY KEY (BlockID,pos) )"

data BlockObjField = BlockObjField Int deriving (Show)

instance FromRow BlockObjField where
  fromRow = BlockObjField <$> field

addCellObject :: Connection -> (ChunkIdx,Index,Index,BlockID) -> IO ()
addCellObject conn ((i,j),k,pos,bid) = execute conn sql (i,j,k,pos,bid)
  where
    sql = "INSERT INTO BlockObjTable ( BlockID , pos, ObjectID ) \
          \ values \
          \ ( \
          \   ( Select BlockID from BlockChunkTable \
          \      where ChunkID = (Select ChunkID from chunktable \
          \                        where i_index = ? and j_index = ?) \
          \            and HPos = ?) \
          \   , ? , ? ) "

getCellObject :: Connection -> (ChunkIdx,Index,Index) -> IO (Maybe BlockID)
getCellObject conn ((i,j),k,pos) = do
  r <- query conn sql (i,j,k,pos) :: IO [BlockObjField]
  case r of
    [] -> return Nothing
    ((BlockObjField v):_) -> return $ Just v
  where
    sql = " Select ObjectID from BlockObjTable \
          \   where \
          \     BlockID = (Select BlockID from BlockChunkTable \
          \       where ChunkID = (Select ChunkID from ChunkTable \
          \               where i_index = ? and j_index = ? ) \
          \             and HPos = ?) \
          \     and Pos = ?"

updateCellObject :: Connection -> (ChunkIdx,Index,Index,BlockID) -> IO ()
updateCellObject conn ((i,j),k,pos,oid) =  execute conn sql (oid,i,j,k,pos) 
  where
    sql = "UPDATE BlockObjTable \
          \ SET ObjectID = ? \
          \   where \
          \     BlockID = (Select BlockID from BlockChunkTable \
          \       where ChunkID = (Select ChunkID from ChunkTable \
          \               where i_index = ? and j_index = ? ) \
          \             and HPos = ?) \
          \     and Pos = ? "

deleteCellObject :: Connection -> (ChunkIdx,Index,Index) -> IO ()
deleteCellObject conn ((i,j),k,pos) =  execute conn sql (i,j,k,pos) 
  where
    sql = "DELETE From BlockObjTable \
          \   where \
          \     BlockID = (Select BlockID from BlockChunkTable \
          \       where ChunkID = (Select ChunkID from ChunkTable \
          \               where i_index = ? and j_index = ? ) \
          \             and HPos = ?) \
          \     and Pos = ? "

data ChunkBlockField = ChunkBlockField Int Int deriving (Show)

instance FromRow ChunkBlockField where
  fromRow = ChunkBlockField <$> field <*> field

getChunkBlock :: Connection -> ChunkIdx -> IO [[(Index,BlockID)]]
getChunkBlock conn (i,j) = mapM (\ k -> do
  r <- query conn sql (i,j,k) :: IO [ChunkBlockField]
  return $ map f2l r)
    [0 .. bkNo]
  where
    !bkNo = blockNum chunkParam - 1
    f2l (ChunkBlockField p v) = (p,v)
    sql = " Select pos,ObjectID from BlockObjTable \
          \   where \
          \     BlockID = (Select BlockID from BlockChunkTable \
          \       where ChunkID = (Select ChunkID from ChunkTable \
          \               where i_index = ? and j_index = ? ) \
          \             and HPos = ?) "

setChunkBlock :: Connection -> ChunkIdx -> [(Index, [(Index,BlockID)])]
              -> IO ()
setChunkBlock _ _ [] = return ()
setChunkBlock conn cidx vlst = mapM_ (setChunkBlock' conn cidx) vlst
 
setChunkBlock' :: Connection -> ChunkIdx -> (Index, [(Index,BlockID)])
               -> IO ()
setChunkBlock' _ _ (_,[]) = return ()
setChunkBlock' conn cidx (k,vlst) = do
  bid <- _getBlockID conn cidx k
  let (vlst',vlst'') = splitAt 200 vlst
  case bid of
    Nothing -> return ()
    Just ci' -> do
      execute_ conn $ sql ci' vlst'
  setChunkBlock' conn cidx (k,vlst'')
  where
    fmt bid (i,oid) = unwords [show bid, ",",  show i, ",", show oid] 
    ufmt bid pos = " union all select " ++ (fmt bid pos )
    rec bid vlst' = concatMap (ufmt bid) $ tail vlst'
    sql bid vlst' = Query $ T.pack $ "INSERT INTO BlockObjTable select "
        ++ (fmt bid $ head vlst') ++ (rec bid vlst') ++ ";"

-- ######### Surface Table ##########

createSurfaceTableSQL :: T.Text
createSurfaceTableSQL =
  "CREATE TABLE SurfaceTable ( \
  \ ChunkID INTEGER NOT NULL, \
  \ pos INTEGER NOT NULL, \
  \ face TEXT NOT NULL, \
  \ PRIMARY KEY (ChunkID,pos) )"

data SurfaceField = SurfaceField String deriving (Show)

instance FromRow SurfaceField where
  fromRow = SurfaceField <$> field

face2str :: [Surface] -> String
face2str fs = map (\ f -> if elem f fs then 'T' else 'F')
                [STop, SBottom, SRight, SLeft, SFront, SBack] 

str2face :: String -> [Surface] 
str2face str = map snd $ filter (\ (s,_) -> s == 'T' ) $ 
                zip str
                    [STop, SBottom, SRight, SLeft, SFront, SBack] 


addSurface :: Connection -> (ChunkIdx,Index,Index,[Surface]) -> IO ()
addSurface _    (_,_,_,[]) = return () 
addSurface conn ((i,j),k,pos,fs) = execute conn sql (i,j,k,pos,tfs)
  where
    sql = "INSERT INTO SurfaceTable ( ChunkID , pos, face ) \
          \ values \
          \ ( \
          \   (Select ChunkID from chunktable \
          \      where i_index = ? and j_index = ? and k_index = ?) \
          \   , ? , ? ) "
    tfs = face2str fs

updateSurface :: Connection -> (ChunkIdx,Index,Index,[Surface]) -> IO ()
updateSurface conn ((i,j),k,pos,fs) =  execute conn sql (tfs,pos,i,j,k) 
  where
    sql = "UPDATE SurfaceTable \
          \ SET face = ? \
          \   where pos = ? \
          \     and ChunkID = (Select ChunkID from ChunkTable \
          \            where  i_index = ? \
          \               and j_index = ? \
          \               and k_index = ? )"
    tfs = face2str fs

deleteSurface :: Connection -> (ChunkIdx,Index,Index) -> IO ()
deleteSurface conn ((i,j),k,pos) =  execute conn sql (pos,i,j,k) 
  where
    sql = "DELETE From SurfaceTable \
          \   where pos = ? \
          \     and ChunkID = (Select ChunkID from ChunkTable \
          \            where  i_index = ? \
          \               and j_index = ? \
          \               and k_index = ? )"

getSurface :: Connection -> (ChunkIdx,Index,Index) -> IO [Surface]
getSurface conn ((i,j),k,pos) = do
  r <- query conn sql (i,j,k,pos) :: IO [SurfaceField]
  case r of
    [] -> return []
    ((SurfaceField v):_) -> return $ str2face v
  where
    sql = " Select face from SurfaceTable \
          \   where ChunkID = (Select ChunkID from ChunkTable \
          \                       where i_index = ? \
          \                         and j_index = ? \
          \                         and k_index = ? ) \
          \         and Pos = ?"

data ChunkSurfaceField = ChunkSurfaceField Int String deriving (Show)

instance FromRow ChunkSurfaceField where
  fromRow = ChunkSurfaceField <$> field <*> field

getChunkSurface :: Connection -> ChunkIdx -> IO [[(Index,[Surface])]]
getChunkSurface conn (i,j) = mapM (\ k -> do
  r <- query conn sql (i,j,k) :: IO [ChunkSurfaceField]
  return $ map f2l r) [ 0 .. bkNo]
  where
    !bkNo = blockNum chunkParam - 1
    sql = " Select pos,face from SurfaceTable \
          \   where ChunkID = (Select ChunkID from ChunkTable \
          \                       where i_index = ? \
          \                         and j_index = ? \
          \                         and k_index = ? )"
    f2l (ChunkSurfaceField p v) = (p, str2face v)

setChunkSurface :: Connection -> ChunkIdx -> [(Index,[(Index,[Surface])])]
                -> IO ()
setChunkSurface _ _ [] = return ()
setChunkSurface conn cidx flsts = mapM_ (setChunkSurface' conn cidx) flsts

setChunkSurface' :: Connection -> ChunkIdx -> (Index, [(Index,[Surface])])
                -> IO ()
setChunkSurface' _ _ (_,[]) = return ()
setChunkSurface' conn cidx (k,flst) = do
  cid <- _getChunkID conn cidx
  let (flst',flst'') = splitAt 200 flst
  case cid of
    Nothing -> return ()
    Just ci' -> do
      execute_ conn $ sql ci' flst'
  setChunkSurface' conn cidx (k,flst'') 
  where
    face2str' fs = "\"" ++ (face2str fs) ++ "\"" 
    fmt cid (i,fs) = unwords [show cid, ",",  show i, ",", face2str' fs] 
    ufmt cid pos = " union all select " ++ (fmt cid pos )
    rec cid vlst' = concatMap (ufmt cid) $ tail vlst'
    sql cid vlst' = Query $ T.pack $ "INSERT INTO SurfaceTable select "
        ++ (fmt cid $ head vlst') ++ (rec cid vlst') ++ ";"

-- #####################################################
--
type ChunkIndex = (Int,Int,Int)

wIndexToChunkpos :: WorldIndex -> (ChunkIndex,Int)
wIndexToChunkpos (x,y,z) = ( (ox,oy,oz) , cidx)
  where
    bsize = blockSize chunkParam
    (ox, lx) = x `divMod` bsize 
    (oy, ly) = y `divMod` bsize
    (oz, lz) = z `divMod` bsize 
    cidx = bsize * bsize * ly + bsize * lz + lx

chunkposToWindex :: (ChunkIndex,Int) -> WorldIndex
chunkposToWindex ((i,j,k),idx) = (x,y,z)
  where
    bsize = blockSize chunkParam
    x = bsize * i + lx
    y = bsize * j + ly
    z = bsize * k + lz
    (ly,t) = idx `divMod` (bsize * bsize)
    (lz,lx) = t `divMod` bsize

-- ######### ObjectInfo Table ##########

createObjInfoTableSQL :: T.Text
createObjInfoTableSQL =
  "CREATE TABLE ObjectInfoTable ( \
  \ ObjectID INTEGER PRIMARY KEY NOT NULL, \
  \ ObjectShape INTEGER NOT NULL)"

addObjInfo :: Connection -> Int -> IO ()
addObjInfo conn i =  execute conn sql (Only i) 
  where
    sql = "INSERT INTO ObjectInfoTable \
          \ ( ObjectShape ) \
          \ values \
          \ (?)"

-- #####################################################

