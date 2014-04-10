{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}

module Hinecraft.WithSqlite
  ( initDB
  , DBHandle
  , exitDB
  )
  where

import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite (runSqlite, runMigrationSilent)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Data.Text (pack)
import Control.Concurrent.Chan
--import Data.Conduit (($$))
--import Data.Conduit.List as CL
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent

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
               | Store [(Idx,Int)]
               | Exit

data DatStream = Dump [(Idx,Int)]

data DBHandle = DBHandle
  { ist :: Chan CmdStream
  , ost :: Chan DatStream 
  }

initDB :: FilePath -> IO DBHandle
initDB home = do
  inst <- newChan
  outst <- newChan
  forkIO $ do
    Dbg.traceIO "start DB Thread"
    runSqlite path (mainLoop inst outst)
    Dbg.traceIO "end DB Thread"
  return $ DBHandle
    { ist = inst
    , ost = outst
    } 
  where
    path = pack $ home ++ "/.Hinecraft/userdata/wld0.db"
    mainLoop inst outst = do
      cmd <- liftIO $ readChan inst
      q <- case cmd of
        Exit -> return True
        Load sidx eidx -> return False
        Put idx v -> return False
        Store lst -> return False
      unless q $ mainLoop inst outst

exitDB :: DBHandle -> IO ()
exitDB hdl = do
  writeChan cmdst Exit
  where
    cmdst = ist hdl

--storeBlksToDB :: [((Int, Int, Int), Int)] -> SqlPersistT m ()
storeBlksToDB blks = do
  runMigrationSilent migrateTables
  insertMany $ map (\ ((x,y,z),v) -> World x y z v) blks
  return ()

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


