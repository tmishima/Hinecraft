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
import Hinecraft.Types
import Hinecraft.DB.Internal

initTable :: Connection -> IO ()
initTable conn = do
  Dbg.traceIO "create Table"
  mapM_ (execute_ conn) 
    [ Query createChunkTableSQL 
    , Query createObjInfoTableSQL
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

mainProcess :: Chan CmdStream -> Chan DatStream -> Connection -> IO ()
mainProcess inst outst conn = do
  cmd <- readChan inst
  q <- case cmd of
      Exit -> return True
      Load cidx -> do
        cflg <- checkChunkData conn cidx
        blks <- if cflg 
          then do
            b' <- getChunkBlock conn cidx 
            return $ map (\ (pos,v) -> (chunkposToWindex (cidx,pos), v)) b' 
          else return []
        writeChan outst $ Dump blks
        Dbg.traceIO "DB: Load"
        return False
      Put (x,y,z) v -> do
        let (cidx,pos) = wIndexToChunkpos (x,y,z)
        cflg <- checkChunkData conn cidx
        unless cflg $ Dbg.trace "DB: add Chunk at Put ope"
                                $ addChunkData conn cidx
        blk <- getBlockPos conn (cidx,pos)
        case blk of
          Nothing -> Dbg.trace "DB: put" $ addBlockPos conn (cidx,pos,v)  
          _ -> Dbg.trace "DB: update" $ updateBlockPos conn (cidx,pos,v) 
        return False
      Del (x,y,z) -> do
        let (cidx,pos) = wIndexToChunkpos (x,y,z)
        cflg <- checkChunkData conn cidx
        if cflg 
          then Dbg.trace "DB: del" $ deleteBlockPos conn (cidx,pos)
          else return ()
        return False
      Store cidx lst -> do
        cflg <- checkChunkData conn cidx
        unless cflg $ Dbg.trace "DB: add Chunk at Put ope"
                                $ addChunkData conn cidx
        setChunkBlock conn cidx $ map (\ (p,v) ->
                                         (snd $ wIndexToChunkpos p,v)) lst
        return False
  unless q $ mainProcess inst outst conn

-- ######### Control ##########

type Idx = (Int,Int,Int)
data CmdStream = Load Idx 
               | Put Idx Int
               | Del Idx
               | Store Idx [(Idx,Int)]
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

setBlockToDB :: DBHandle -> Idx -> Int -> IO ()
setBlockToDB hdl idx val = do
  writeChan cmdst $ Put idx val
  where
    cmdst = ist hdl

readChunkData :: DBHandle -> Idx -> IO [(WorldIndex,Int)]
readChunkData hdl chnkID = do 
  writeChan cmdst $ Load chnkID 
  Dump blks <- readChan dst
  return blks 
  where
    cmdst = ist hdl
    dst = ost hdl

delBlockInDB :: DBHandle -> Idx -> IO ()
delBlockInDB hdl idx = do
  writeChan cmdst $ Del idx
  where
    cmdst = ist hdl

writeChunkData :: DBHandle -> Idx -> [(Idx,Int)] -> IO ()
writeChunkData _ _ [] = return ()
writeChunkData hdl cidx lst = do
  writeChan cmdst $ Store cidx lst
  where
    cmdst = ist hdl

