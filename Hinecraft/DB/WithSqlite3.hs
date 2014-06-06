{-# LANGUAGE OverloadedStrings #-}

module Hinecraft.DB.WithSqlite3
{-  ( initDB
  , DBHandle
  , exitDB
  , readChunkData
  , writeChunkData
  , setBlockToDB
  , delBlockInDB
  ) -}
  where

import Control.Applicative
import Database.SQLite.Simple

import System.Directory
import Control.Exception
import Control.Monad
import qualified Data.Text as T
--import Database.SQLite.Simple.FromRow

type ChunkIdx = (Int,Int,Int)
type BlockID = Int
type Index = Int

-- for Unit test debug

dbfunc1 :: Int
dbfunc1 = 1

dbfunc2 :: IO Bool
dbfunc2 = return True

-- ######### Chunk Table ##########

data ChunkIDField = ChunkIDField Int deriving (Show)

instance FromRow ChunkIDField where
  fromRow = ChunkIDField <$> field

createChunkTableSQL :: T.Text
createChunkTableSQL =
  "CREATE TABLE ChunkTable ( \
  \ i_index INTEGER NOT NULL, \
  \ j_index INTEGER NOT NULL, \
  \ k_index INTEGER NOT NULL, \
  \ ChunkID INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL )"

addChunkData :: Connection -> ChunkIdx -> IO ()
addChunkData conn (i,j,k) = execute conn sql (i,j,k) 
  where
    sql = "INSERT INTO ChunkTable \
          \ ( i_index , j_index , k_index ) \
          \ values \
          \ (?, ?, ?)"

checkChunkData :: Connection -> ChunkIdx -> IO Bool
checkChunkData conn (i,j,k) = do
  r <- query conn sql (i,j,k) :: IO [ChunkIDField]
  return $ case r of
    [] -> False 
    _ -> True
  where
    sql = " Select ChunkID from ChunkTable \
          \   where i_index = ? \
          \     and j_index = ? \
          \     and k_index = ? "


-- ######### BlockInfo Table ##########

createBlockInfoTableSQL :: T.Text
createBlockInfoTableSQL =
  "CREATE TABLE BlockInfoTable ( \
  \ BlockID INTEGER PRIMARY KEY NOT NULL, \
  \ BlockShape INTEGER NOT NULL)"

addBlockInfo :: Connection -> Int -> IO ()
addBlockInfo conn i =  execute conn sql (Only i) 
  where
    sql = "INSERT INTO BlockInfoTable \
          \ ( BlockShape ) \
          \ values \
          \ (?)"


-- ######### BlockPos Table ##########

createBlockPosTableSQL :: T.Text
createBlockPosTableSQL =
  "CREATE TABLE BlockPosTable ( \
  \ ChunkID INTEGER NOT NULL, \
  \ pos INTEGER NOT NULL, \
  \ BlockID INTEGER NOT NULL, \
  \ PRIMARY KEY (ChunkID,pos) )"

data BlockPosField = BlockPosField Int deriving (Show)

instance FromRow BlockPosField where
  fromRow = BlockPosField <$> field

addBlockPos :: Connection -> (ChunkIdx,Index,BlockID) -> IO ()
addBlockPos conn ((i,j,k),pos,bid) = execute conn sql (i,j,k,pos,bid)
  where
    sql = "INSERT INTO BlockPosTable ( ChunkID , pos, BlockID ) \
          \ values \
          \ ( \
          \   (Select ChunkID from chunktable \
          \      where i_index = ? and j_index = ? and k_index = ?) \
          \   , ? , ? ) "

updateBlockPos :: Connection -> (ChunkIdx,Index,BlockID) -> IO ()
updateBlockPos conn ((i,j,k),pos,bid) =  execute conn sql (bid,pos,i,j,k) 
  where
    sql = "UPDATE BlockPosTable \
          \ SET BlockID = ? \
          \   where pos = ? \
          \     and ChunkID = (Select ChunkID from ChunkTable \
          \            where  i_index = ? \
          \               and j_index = ? \
          \               and k_index = ? )"

deleteBlockPos :: Connection -> (ChunkIdx,Index) -> IO ()
deleteBlockPos conn ((i,j,k),pos) =  execute conn sql (pos,i,j,k) 
  where
    sql = "DELETE From BlockPosTable \
          \   where pos = ? \
          \     and ChunkID = (Select ChunkID from ChunkTable \
          \            where  i_index = ? \
          \               and j_index = ? \
          \               and k_index = ? )"

getBlockPos :: Connection -> (ChunkIdx,Index) -> IO (Maybe BlockID)
getBlockPos conn ((i,j,k),pos) = do
  r <- query conn sql (i,j,k,pos) :: IO [BlockPosField]
  case r of
    [] -> return Nothing
    ((BlockPosField v):_) -> return $ Just v
  where
    sql = " Select BlockID from BlockPosTable \
          \   where ChunkID = (Select ChunkID from ChunkTable \
          \                       where i_index = ? \
          \                         and j_index = ? \
          \                         and k_index = ? ) \
          \         and Pos = ?"

data ChunkBlockField = ChunkBlockField Int Int deriving (Show)

instance FromRow ChunkBlockField where
  fromRow = ChunkBlockField <$> field <*> field

getChunkBlock :: Connection -> ChunkIdx -> IO [(Index,BlockID)]
getChunkBlock conn (i,j,k) = do
  r <- query conn sql (i,j,k) :: IO [ChunkBlockField]
  return $ map f2l r
  where
    sql = " Select pos,BlockID from BlockPosTable \
          \   where ChunkID = (Select ChunkID from ChunkTable \
          \                       where i_index = ? \
          \                         and j_index = ? \
          \                         and k_index = ? )"
    f2l (ChunkBlockField p v) = (p,v)

{-
setBlocksToChunk :: Connection -> ChunkIdx -> [(Index,BlockID)] -> IO ()
setBlocksToChunk _ _ [] = return ()
addBlocksToChunk conn cidx vlst = undefined

  execute conn sql (i,j,k)
  where
    vlst' = take 200 vlst
    sql = "INSERT INTO BlockPosTable \
          \ ( ChunkID, pos BlockID ) \
          \ values \
          \ (?,?)
          "
-}


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

data Surface = STop | SBottom | SRight | SLeft | SFront | SBack
  deriving (Show,Eq,Ord)

face2str :: [Surface] -> String
face2str fs = map (\ f -> if elem f fs then 'T' else 'F')
                [STop, SBottom, SRight, SLeft, SFront, SBack] 

str2face :: String -> [Surface] 
str2face str = map snd $ filter (\ (s,f) -> s == 'T' ) $ 
                zip str
                    [STop, SBottom, SRight, SLeft, SFront, SBack] 


addSurface :: Connection -> (ChunkIdx,Index,[Surface]) -> IO ()
addSurface conn ((i,j,k),pos,[]) = return () 
addSurface conn ((i,j,k),pos,fs) = execute conn sql (i,j,k,pos,tfs)
  where
    sql = "INSERT INTO SurfaceTable ( ChunkID , pos, face ) \
          \ values \
          \ ( \
          \   (Select ChunkID from chunktable \
          \      where i_index = ? and j_index = ? and k_index = ?) \
          \   , ? , ? ) "
    tfs = face2str fs

updateSurface :: Connection -> (ChunkIdx,Index,[Surface]) -> IO ()
updateSurface conn ((i,j,k),pos,fs) =  execute conn sql (tfs,pos,i,j,k) 
  where
    sql = "UPDATE SurfaceTable \
          \ SET face = ? \
          \   where pos = ? \
          \     and ChunkID = (Select ChunkID from ChunkTable \
          \            where  i_index = ? \
          \               and j_index = ? \
          \               and k_index = ? )"
    tfs = face2str fs

deleteSurface :: Connection -> (ChunkIdx,Index) -> IO ()
deleteSurface conn ((i,j,k),pos) =  execute conn sql (pos,i,j,k) 
  where
    sql = "DELETE From SurfaceTable \
          \   where pos = ? \
          \     and ChunkID = (Select ChunkID from ChunkTable \
          \            where  i_index = ? \
          \               and j_index = ? \
          \               and k_index = ? )"

getSurface :: Connection -> (ChunkIdx,Index) -> IO [Surface]
getSurface conn ((i,j,k),pos) = do
  r <- query conn sql (i,j,k,pos) :: IO [SurfaceField]
  case r of
    [] -> return []
    ((SurfaceField v):_) -> return $ str2face v
  where
    sql = " Select BlockID from SurfaceTable \
          \   where ChunkID = (Select ChunkID from ChunkTable \
          \                       where i_index = ? \
          \                         and j_index = ? \
          \                         and k_index = ? ) \
          \         and Pos = ?"

data ChunkSurfaceField = ChunkSurfaceField Int String deriving (Show)

instance FromRow ChunkSurfaceField where
  fromRow = ChunkSurfaceField <$> field <*> field

getChunkSurface :: Connection -> ChunkIdx -> IO [(Index,[Surface])]
getChunkSurface conn (i,j,k) = do
  r <- query conn sql (i,j,k) :: IO [ChunkSurfaceField]
  return $ map f2l r
  where
    sql = " Select pos,BlockID from SurfaceTable \
          \   where ChunkID = (Select ChunkID from ChunkTable \
          \                       where i_index = ? \
          \                         and j_index = ? \
          \                         and k_index = ? )"
    f2l (ChunkSurfaceField p v) = (p, str2face v)

-- ######### Control ##########

dbProcess :: IO ()
dbProcess = bracket (initProcess "test.db")
                    exitProcess
                    mainProcess  


mainProcess :: Connection -> IO ()
mainProcess conn = do
  let chnkID = (1,2,3)
  print "start main"
  chk <- checkChunkData conn chnkID
  unless chk $ do
    print "add chnuk"
    addChunkData conn chnkID 
  p1 <- getBlockPos conn (chnkID,4)
  case p1 of
    Just v -> (print . show) v
    Nothing -> do 
      addBlockPos conn (chnkID,4,5)
      print "add blk 4"
  p2 <- getBlockPos conn (chnkID,0)
  case p2 of
    Just v -> (print . show) v
    Nothing -> do
      addBlockPos conn (chnkID,0,5)
      print "add blk 5"
  -- 
  blks <- getChunkBlock conn chnkID
  print $ show blks
  updateBlockPos conn (chnkID,4,2)
  deleteBlockPos conn (chnkID,0)

  print "end main"
  return ()


initDB :: Connection -> IO ()
initDB conn = do
  print "create Table"
  mapM_ (execute_ conn) 
    [ Query createChunkTableSQL 
    , Query createBlockInfoTableSQL
    , Query createBlockPosTableSQL
    , Query createSurfaceTableSQL
    ]
  print "finish create Table "

initProcess :: FilePath -> IO Connection
initProcess dbPath = do
  print "start DB Process"
  e <- doesFileExist dbPath
  conn <- open dbPath
  unless e $ initDB conn
  return conn

exitProcess :: Connection -> IO ()
exitProcess conn = do
  close conn
  print "exit DB Process"

{-
data TestField = TestField Int String deriving (Show)

instance FromRow TestField where
  fromRow = TestField <$> field <*> field

main :: IO ()
main = do
  conn <- open "test.db"
  execute conn "INSERT INTO test (str) VALUES (?)" (Only ("test string 2" :: String))
  r <- query_ conn "SELECT * from test" :: IO [TestField]
  mapM_ print r
  close conn
-}


