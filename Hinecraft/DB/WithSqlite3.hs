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
import Control.Concurrent.Chan
import Control.Concurrent

import Debug.Trace as Dbg

type ChunkIdx = (Int,Int,Int)
type BlockID = Int
type Index = Int

-- for Unit test debug

dbfunc1 :: Int
dbfunc1 = 1

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

_getChunkID :: Connection -> ChunkIdx -> IO (Maybe Int)
_getChunkID conn (i,j,k) = do
  r <- query conn sql (i,j,k) :: IO [ChunkIDField]
  return $ case r of
    [] -> Nothing
    ((ChunkIDField chi):_) -> Just chi 
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


setChunkBlock :: Connection -> ChunkIdx -> [(Index,BlockID)] -> IO ()
setChunkBlock _ _ [] = return ()
setChunkBlock conn cidx vlst = do
  cid <- _getChunkID conn cidx
  let (vlst',vlst'') = splitAt 200 vlst
  case cid of
    Nothing -> return ()
    Just ci' -> do
      execute_ conn $ sql ci' vlst'
  setChunkBlock conn cidx vlst'' 
  where
    fmt cid (i,bid) = unwords [show cid, ",",  show i, ",", show bid] 
    ufmt cid pos = " union all select " ++ (fmt cid pos )
    rec cid vlst' = concatMap (ufmt cid) $ tail vlst'
    sql cid vlst' = Query $ T.pack $ "INSERT INTO BlockPosTable select "
        ++ (fmt cid $ head vlst') ++ (rec cid vlst') ++ ";"

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
str2face str = map snd $ filter (\ (s,_) -> s == 'T' ) $ 
                zip str
                    [STop, SBottom, SRight, SLeft, SFront, SBack] 


addSurface :: Connection -> (ChunkIdx,Index,[Surface]) -> IO ()
addSurface _    (_,_,[]) = return () 
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
    sql = " Select face from SurfaceTable \
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
    sql = " Select pos,face from SurfaceTable \
          \   where ChunkID = (Select ChunkID from ChunkTable \
          \                       where i_index = ? \
          \                         and j_index = ? \
          \                         and k_index = ? )"
    f2l (ChunkSurfaceField p v) = (p, str2face v)

setChunkSurface :: Connection -> ChunkIdx -> [(Index,[Surface])]
                -> IO ()
setChunkSurface _ _ [] = return ()
setChunkSurface conn cidx flst = do
  cid <- _getChunkID conn cidx
  let (flst',flst'') = splitAt 200 flst
  case cid of
    Nothing -> return ()
    Just ci' -> do
      execute_ conn $ sql ci' flst'
  setChunkSurface conn cidx flst'' 
  where
    face2str' fs = "\"" ++ (face2str fs) ++ "\"" 
    fmt cid (i,fs) = unwords [show cid, ",",  show i, ",", face2str' fs] 
    ufmt cid pos = " union all select " ++ (fmt cid pos )
    rec cid vlst' = concatMap (ufmt cid) $ tail vlst'
    sql cid vlst' = Query $ T.pack $ "INSERT INTO SurfaceTable select "
        ++ (fmt cid $ head vlst') ++ (rec cid vlst') ++ ";"

initTable :: Connection -> IO ()
initTable conn = do
  Dbg.traceIO "create Table"
  mapM_ (execute_ conn) 
    [ Query createChunkTableSQL 
    , Query createBlockInfoTableSQL
    , Query createBlockPosTableSQL
    , Query createSurfaceTableSQL
    ]
  Dbg.traceIO "finish create Table "

initProcess :: FilePath -> IO Connection
initProcess dbPath = do
  Dbg.traceIO "start DB Process"
  e <- doesFileExist dbPath
  conn <- open dbPath
  unless e $ initTable conn
  return conn

exitProcess :: Chan DatStream -> Connection -> IO ()
exitProcess outst conn = do
  close conn
  writeChan outst Finish
  Dbg.traceIO "exit DB Process"

-- ######### Control ##########

type Idx = (Int,Int,Int)
data CmdStream = Load Idx Idx
               | Put Idx Int
               | Del Idx
               | Store [(Idx,Int)]
               | Exit

data DatStream = Dump [(Idx,Int)]
               | Finish

data DBHandle = DBHandle
  { ist :: Chan CmdStream
  , ost :: Chan DatStream 
  }

initDB :: FilePath -> IO DBHandle
initDB home = do
  inst <- newChan
  outst <- newChan
  _ <- forkIO $ do
    Dbg.traceIO "start DB Thread"
    bracket (initProcess file)
            (exitProcess outst)
            (mainProcess inst outst)
    Dbg.traceIO "end DB Thread"
  return $! DBHandle
    { ist = inst
    , ost = outst
    } 
  where
    file = if null home 
      then ":memory:"
      else home ++ "/.Hinecraft/userdata/wld0.db"

mainProcess :: Chan CmdStream -> Chan DatStream -> Connection -> IO ()
mainProcess inst outst conn = do
  cmd <- readChan inst
  q <- case cmd of
      Exit -> return True
      Load sidx eidx -> do
        --blks <- loadBlksFromDB sidx eidx
        --liftIO $ writeChan outst $ Dump blks
        --liftIO $ Dbg.traceIO "DB Load"
        return False
      Put (x,y,z) v -> do

        return False
      Del (x,y,z) -> do
        --blk <- selectList [ WorldX ==. x, WorldY ==. y, WorldZ ==. z] []
        --unless (null blk) $ do
        --  delete (entityKey $ head blk)
        --  --liftIO $ Dbg.traceIO $ "DB Del = " ++ show (x,y,z)
        --  return ()
        return False
      Store lst -> do
        --liftIO $ Dbg.traceIO "Store"
        --_ <- insertMany $ map (\ ((x,y,z),v) -> World x y z v) lst
        return False
  unless q $ mainProcess inst outst conn


exitDB :: DBHandle -> IO ()
exitDB hdl = do
  writeChan cmdst Exit
  Dbg.traceIO "DB Finish Wait"
  waitLoop 
  where
    waitLoop = do
      r <- readChan dst 
      f <- case r of
        Finish -> return True
        _ -> return False
      unless f waitLoop
    cmdst = ist hdl
    dst = ost hdl

