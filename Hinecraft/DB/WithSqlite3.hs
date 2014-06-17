{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

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
      Load (i,k) -> do
        cflg <- checkChunk conn (i,k)
        (blks,sufs) <- if cflg 
          then do
            b' <- getChunkBlock conn (i,k) 
            f' <- getChunkSurface conn (i,k)
            return 
              ( map (\ (j,b'') ->
                  map (\ (pos,v) -> (chunkposToWindex ((i,j,k),pos), v)) b''
                  ) $ zip [0..bkNo-1] b' 
              ,  map (\ (j,f'') ->
                  map (\ (pos,v) -> (chunkposToWindex ((i,j,k),pos), v)) f''
                  ) $ zip [0..bkNo-1] f' 
              )
          else return (replicate bkNo [], replicate bkNo [])
        writeChan outst $ Dump blks sufs
        Dbg.traceIO "DB: Load"
        return False
      Put (x,y,z) v fs -> do
        let ((i,j,k),pos) = wIndexToChunkpos (x,y,z)
        --Dbg.traceIO $ show ((i,j,k),pos)
        cflg <- checkChunk conn (i,k)
        unless cflg $ Dbg.trace "DB: add Chunk at Put ope"
                    $ addChunk' conn (i,k)
        blk <- getCellObject conn ((i,k),j,pos)
        case blk of
          Nothing -> Dbg.trace "DB: put obj"
                       $ addCellObject conn ((i,k),j,pos,v)  
          _ -> Dbg.trace "DB: update obj"
                       $ updateCellObject conn ((i,k),j,pos,v) 
        unless (null fs) $ do
          suf <- getSurface conn ((i,k),j,pos)
          if null suf 
            then Dbg.trace "DB: put suf"
                       $ addSurface conn ((i,k),j,pos,fs)  
            else Dbg.trace "DB: update suf"
                       $ updateSurface conn ((i,k),j,pos,fs) 
        return False
      Del (x,y,z) -> do
        let ((i,j,k),pos) = wIndexToChunkpos (x,y,z)
        cflg <- checkChunk conn (i,k) 
        if cflg 
          then Dbg.trace "DB: del" $ do
            deleteCellObject conn ((i,k),j,pos)
            deleteSurface conn ((i,k),j,pos)
          else return ()
        return False
      Store (i,k) blks sufs -> do 
        cflg <- checkChunk conn (i,k)
        unless cflg $ Dbg.trace "DB: add Chunk at Put ope"
                    $ addChunk' conn (i,k)
        setChunkBlock conn (i,k) $ zip [0 .. (bkNo-1)] 
          $ map (map (\ (p,v) ->
                   (snd $ wIndexToChunkpos p,v))) blks
        setChunkSurface conn (i,k) $ zip [0 .. (bkNo-1)] 
          $ map (map (\ (p,v) ->
                   (snd $ wIndexToChunkpos p,v))) sufs

        return False
  unless q $ mainProcess inst outst conn
  where
    !bkNo = blockNum chunkParam
    addChunk' conn (i,k) = do
      addChunk conn (i,k)
      mapM_ (\ j -> addBlockToChunk conn ((i,k),j)) [0 .. (bkNo - 1)]

-- ######### Control ##########

type Idx = (Int,Int,Int)
data CmdStream = Load ChunkIdx 
               | Put Idx Int [Surface]
               | Del Idx
               | Store ChunkIdx [[(Idx,Int)]] [[(Idx,[Surface])]]
               | Exit

data DatStream = Dump [[(Idx,Int)]] [[(Idx,[Surface])]]
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

setBlockToDB :: DBHandle -> Idx -> Int -> [Surface] -> IO ()
setBlockToDB hdl idx val fs = do
  writeChan cmdst $ Put idx val fs 
  where
    cmdst = ist hdl

readChunkData :: DBHandle -> ChunkIdx
              -> IO ([[(WorldIndex,Int)]],[[(WorldIndex,[Surface])]])
readChunkData hdl chnkID = do 
  writeChan cmdst $ Load chnkID 
  Dump blks sufs <- readChan dst
  return (blks , sufs)
  where
    cmdst = ist hdl
    dst = ost hdl

delBlockInDB :: DBHandle -> Idx -> IO ()
delBlockInDB hdl idx = do
  writeChan cmdst $ Del idx
  where
    cmdst = ist hdl

writeChunkData :: DBHandle -> ChunkIdx -> [[(Idx,Int)]]
               -> [[(Idx,[Surface])]]
               -> IO ()
writeChunkData _ _ [] _ = return ()
writeChunkData hdl cidx lst fs = do
  writeChan cmdst $ Store cidx lst fs
  where
    cmdst = ist hdl

