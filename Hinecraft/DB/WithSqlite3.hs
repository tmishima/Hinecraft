{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
--
-- Copyright : (c) T.Mishima 2014
-- License : Apache-2.0
--
module Hinecraft.DB.WithSqlite3
  ( DBHandle
  , initDB
  , exitDB
  , readChunkData
  , writeChunkData
  , setObjectAtCell
  , delObjectAtCell
  , writeSurfaceBlock
  ) 
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
    , Query createBlockChunkTableSQL
    , Query createObjInfoTableSQL
    , Query createBlockObjTableSQL
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
      Load (i,k) -> withTransaction conn $ do
        cflg <- checkChunk conn (i,k)
        (blks,sufs) <- if cflg 
          then do
            b' <- getChunkBlock conn (i,k) 
            f' <- getChunkSurface conn (i,k)
            return ( b', f' )
          else return (replicate bkNo [], replicate bkNo [])
        writeChan outst $ Dump blks sufs
        -- Dbg.traceIO $ unwords ["DB: Cell Chunk =",show (i,k)," Load" ]
        return False
      PutCell (i,k) j pos v -> do
        cflg <- checkChunk conn (i,k)
        unless cflg -- $ Dbg.trace "DB: add Chunk at Put ope"
                    $ addChunk' conn (i,k)
        blk <- getCellObject conn ((i,k),j,pos)
        case blk of
          Nothing -> --Dbg.trace "DB: put obj" $
                        addCellObject conn ((i,k),j,pos,v)  
          _ -> -- Dbg.trace "DB: update obj" $
                        updateCellObject conn ((i,k),j,pos,v) 
        return False
      DelCell (i,k) j pos -> do
        cflg <- checkChunk conn (i,k) 
        if cflg 
          then -- Dbg.trace "DB: del" $ 
            deleteCellObject conn ((i,k),j,pos)
          else return ()
        return False
      Store (i,k) blks sufs -> withTransaction conn $ do 
        cflg <- checkChunk conn (i,k)
        unless cflg -- $ Dbg.trace "DB: add Chunk at Store ope" 
                    $ addChunk' conn (i,k)
        setChunkBlock conn (i,k) $ zip [0 .. (bkNo-1)] blks
        setChunkSurface conn (i,k) $ zip [0 .. (bkNo-1)] sufs
        return False
      GetSBlk (i,k) j -> withTransaction conn $ do
        cflg <- checkChunk conn (i,k)
        sufs <- if cflg 
          then getSurfaceBlock conn ((i,k),j)
          else return []
        writeChan outst $ DumpSB sufs
        -- Dbg.traceIO $ unwords ["DB: SBlock =",show (i,k,j)," Load" ]
        return False
      PutSBlk (i,k) j sufs -> withTransaction conn $ do
        cflg <- checkChunk conn (i,k)
        unless cflg -- $ Dbg.trace "DB: add SurfaceChunk at PutSBlk ope"
                    $ addChunk' conn (i,k)
        deleteSurfaceBlock conn ((i,k),j) 
        addSurfaceBlock conn ((i,k),j) sufs
        return False

  unless q $ mainProcess inst outst conn
  where
    !bkNo = blockNum chunkParam
    addChunk' conn (i,k) = do
      addChunk conn (i,k)
      mapM_ (\ j -> addBlockToChunk conn ((i,k),j)) [0 .. (bkNo - 1)]

-- ######### Interface ##########

data CmdStream = Load ChunkIdx 
               | PutCell (Int,Int) Int Int Int
               | DelCell (Int,Int) Int Int
               | Store ChunkIdx [[(Int,Int)]] [[(Int,[Surface])]]
               | PutSBlk (Int,Int) Int [(Int,[Surface])]
               | GetSBlk (Int,Int) Int 
               | Exit

data DatStream = Dump [[(Int,Int)]] [[(Int,[Surface])]]
               | DumpSB [(Int,[Surface])]
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

setObjectAtCell :: DBHandle -> ChunkIdx -> Int -> Int -> Int
            -> IO ()
setObjectAtCell hdl cidx bidx pos val = do
  writeChan cmdst $ PutCell cidx bidx pos val 
  where
    cmdst = ist hdl

readChunkData :: DBHandle -> ChunkIdx
              -> IO ([[(Int,Int)]],[[(Int,[Surface])]])
readChunkData hdl chnkID = do 
  writeChan cmdst $ Load chnkID 
  Dump blks sufs <- readChan dst
  return (blks , sufs)
  where
    cmdst = ist hdl
    dst = ost hdl

readSurfaceBlock :: DBHandle -> ChunkIdx -> Int
              -> IO [(Int,[Surface])]
readSurfaceBlock hdl chnkID blkNo = do 
  writeChan cmdst $ GetSBlk chnkID blkNo
  DumpSB sufs <- readChan dst
  return sufs
  where
    cmdst = ist hdl
    dst = ost hdl

writeSurfaceBlock :: DBHandle -> ChunkIdx -> Int
                  -> [(Int,[Surface])] -> IO ()
writeSurfaceBlock hdl cidx blkNo fs = do 
  writeChan cmdst $ PutSBlk cidx blkNo fs
  where
    cmdst = ist hdl

delObjectAtCell :: DBHandle -> ChunkIdx -> Int -> Int -> IO ()
delObjectAtCell hdl cidx bidx pos = do
  writeChan cmdst $ DelCell cidx bidx pos
  where
    cmdst = ist hdl

writeChunkData :: DBHandle -> ChunkIdx -> [[(Int,Int)]]
               -> [[(Int,[Surface])]]
               -> IO ()
writeChunkData _ _ [] _ = return ()
writeChunkData hdl cidx lst fs = do
  writeChan cmdst $ Store cidx lst fs
  where
    cmdst = ist hdl

