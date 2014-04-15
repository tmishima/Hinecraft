{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}

module Hinecraft.WithSqlite
  ( initDB
  , DBHandle
  , exitDB
  , readChunkData
  , writeChunkData
  , setBlockToDB
  , delBlockInDB
  )
  where

import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite (runSqlite) --, runMigrationSilent)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Data.Text (pack)
import Control.Concurrent.Chan
--import Data.Conduit (($$))
--import Data.Conduit.List as CL
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent
import System.Directory

import Hinecraft.Types

import Debug.Trace as Dbg

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
World
  x       Int
  y       Int
  z       Int
  blockID Int
  deriving  Show
|]

type Idx = (Int,Int,Int)
data CmdStream = Load Idx Idx
               | Put Idx Int
               | Del Idx
               | Store [(Idx,Int)]
               | Exit
               | Init

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
  ef <- doesFileExist file
  unless ef $ writeChan inst Init
  _ <- forkIO $ do
    Dbg.traceIO "start DB Thread"
    runSqlite path $ do
      _ <- runMigrationSilent migrateTables
      (mainLoop inst outst)
    writeChan outst Finish
    Dbg.traceIO "end DB Thread"
  return $! DBHandle
    { ist = inst
    , ost = outst
    } 
  where
    file = home ++ "/.Hinecraft/userdata/wld0.db"
    path = pack $ file
    mainLoop inst outst = do
      cmd <- liftIO $ readChan inst
      q <- case cmd of
        Init -> do
          --liftIO $ Dbg.traceIO "DB Init"
          return False
        Exit -> return True
        Load sidx eidx -> do
          blks <- loadBlksFromDB sidx eidx
          liftIO $ writeChan outst $ Dump blks
          --liftIO $ Dbg.traceIO "DB Load"
          return False
        Put (x,y,z) v -> do
          blk <- selectList [ WorldX ==. x, WorldY ==. y, WorldZ ==. z] []
          if null blk
            then do
              _ <- insert $ World x y z v
              --liftIO $ Dbg.traceIO $ "DB insert = " ++ show (x,y,z,v)
              return ()
            else do
              update (entityKey $ head blk) [ WorldBlockID =. v ]
              --liftIO $ Dbg.traceIO $ "DB Put = " ++ show (x,y,z,v)
              return ()
          return False
        Del (x,y,z) -> do
          blk <- selectList [ WorldX ==. x, WorldY ==. y, WorldZ ==. z] []
          unless (null blk) $ do
            delete (entityKey $ head blk)
            --liftIO $ Dbg.traceIO $ "DB Del = " ++ show (x,y,z)
            return ()
          return False
        Store lst -> do
          --liftIO $ Dbg.traceIO "Store"
          _ <- insertMany $ map (\ ((x,y,z),v) -> World x y z v) lst
          return False
      unless q $ mainLoop inst outst

setBlockToDB :: DBHandle -> Idx -> Int -> IO ()
setBlockToDB hdl idx val = do
  writeChan cmdst $ Put idx val
  where
    cmdst = ist hdl

delBlockInDB :: DBHandle -> Idx -> IO ()
delBlockInDB hdl idx = do
  writeChan cmdst $ Del idx
  where
    cmdst = ist hdl

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

readChunkData :: DBHandle -> (Int,Int) -> IO [[(Idx,Int)]]
readChunkData hdl (i,j) = mapM (\ (st,ed) -> do
    writeChan cmdst $ Load st ed 
    Dump blks <- readChan dat  
    return blks) genIdxLst
  where
    cmdst = ist hdl
    dat = ost hdl
    genIdxLst = map (\ k -> ( ( i * bsize, k * bsize, j * bsize)
                            , ( (i + 1) * bsize - 1, (k + 1) * bsize - 1
                              , (j + 1) * bsize - 1)
                            )
                    ) [0 .. bnum - 1]
    bsize = blockSize chunkParam
    bnum = blockNum chunkParam

writeChunkData :: DBHandle -> [(Idx,Int)] -> IO ()
writeChunkData hdl lst = do
  writeChan cmdst $ Store lst
  where
    cmdst = ist hdl

loadBlksFromDB :: PersistQuery m
               => (Int, Int, Int) -> (Int, Int, Int)
               -> m [((Int,Int,Int),Int)]
loadBlksFromDB (sx,sy,sz) (ex,ey,ez) = do
  blks <- selectList [ WorldX >=. sx, WorldX <=. ex
                     , WorldY >=. sy, WorldY <=. ey
                     , WorldZ >=. sz, WorldZ <=. ez
                     ] []
  return $ map ((\ (World x y z v) -> ((x,y,z),v)) . entityVal) blks

getBlkFromDB :: PersistQuery m => (Int, Int, Int) -> m (Maybe Int)
getBlkFromDB (x,y,z) = do
  blks <- selectList [ WorldX ==. x, WorldY ==. y, WorldZ ==. z] []
  case blks of
    [] -> return $ Nothing
    (b : _) -> return $ Just $ worldBlockID $ entityVal b

updateBlkInDB :: PersistQuery m => (Int, Int, Int) -> Int -> m Bool
updateBlkInDB (x,y,z) v = do
  blks <- selectList [ WorldX ==. x, WorldY ==. y, WorldZ ==. z] []
  case blks of
    [] -> return False
    (b:_) -> update (entityKey b) [ WorldBlockID =. v ]
               >> return True


