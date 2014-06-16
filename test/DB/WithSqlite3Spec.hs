{-# LANGUAGE OverloadedStrings #-} 

module DB.WithSqlite3Spec where

import Test.Hspec
--import Database.SQLite.Simple

import Hinecraft.DB.WithSqlite3
import Hinecraft.DB.Internal
import Hinecraft.Types
import Control.Concurrent.Chan

main :: IO()
main = hspec spec

spec :: Spec
spec = do
  specAB
  -- specCD

withConnect fn = do
  tmp <- newChan 
  --conn <- initProcess ":memory:"
  conn <- initProcess "tmp.db"
  ret <- fn conn
  exitProcess tmp conn 
  return ret

test1 = withConnect (\ conn -> do
  chk1 <- checkChunkData conn chnkID 
  addChunkData conn chnkID 
  Just cid <- _getChunkID conn chnkID
  chk2 <- checkChunkData conn chnkID 
  addChunkData conn chnkID 
  chk3 <- checkChunkData conn chnkID 
  return $ and [not chk1, chk2, chk3,cid == 1])
  where
    chnkID = (0,0) 

test2 = withConnect (\ conn -> do
  addChunkData conn chnkID 
  p1 <- getBlockPos conn (chnkID,4)
  addBlockPos conn (chnkID,4,5)
  p2 <- getBlockPos conn (chnkID,4)
  flg <- return $ case p2 of
    Just v -> v == 5
    Nothing -> False 
  return $ and [p1 == Nothing, flg])
  where
    chnkID = (0,0,0) 

{-
test2' = withConnect (\ conn -> do
  addChunkData conn chnkID 
  setChunkBlock conn chnkID
    $ zip [1..1000] [11 .. ]
  Just p1 <- getBlockPos conn (chnkID,1)
  Just p2 <- getBlockPos conn (chnkID,200)
  Just p3 <- getBlockPos conn (chnkID,201)
  Just p4 <- getBlockPos conn (chnkID,1000)
  return $ and [p1 == 11, p2 == 210, p3 == 211, p4 == 1010])
  where
    chnkID = (0,0,0) 

test2'' = withConnect (\ conn -> do
  addChunkData conn chnkID 
  setChunkBlock conn chnkID dat
  blst <- getChunkBlock conn chnkID
  return $ and $ map (\ (x,y) -> x == y ) $ zip dat blst)
  where
    chnkID = (0,0,0) 
    dat = zip [1..1000] [11 .. ]


test3 = withConnect (\ conn -> do
  addChunkData conn chnkID 
  addBlockPos conn (chnkID,4,5)
  updateBlockPos conn (chnkID,4,2)
  p1 <- getBlockPos conn (chnkID,4)
  flg <- return $ case p1 of
    Just v -> v == 2
    Nothing -> False 
  return flg)
  where
    chnkID = (0,0,0) 

test4 = withConnect (\ conn -> do
  addChunkData conn chnkID 
  addBlockPos conn (chnkID,4,5)
  deleteBlockPos conn (chnkID,4)
  p1 <- getBlockPos conn (chnkID,4)
  flg <- return $ case p1 of
    Just v -> False
    Nothing -> True 
  return flg)
  where
    chnkID = (0,0,0) 

test5 = withConnect (\ conn -> do
  addChunkData conn chnkID 
  addSurface conn (chnkID,4,[STop]) 
  f1 <- getSurface conn (chnkID,4)
  flg1 <- return $ case f1 of
    [STop] -> True
    _ -> False 

  updateSurface conn (chnkID,4,suf) 
  f2 <- getSurface conn (chnkID,4)
  flg2 <- return $ fst $ chk f2

  deleteSurface conn (chnkID,4) 
  f3 <- getSurface conn (chnkID,4)
  flg3 <- return $ f3 == []

  return $ and [flg1, flg2, flg3])
  where
    chnkID = (0,0,0) 
    suf = [SLeft,SRight]
    chk = foldr (\ f (b,ft) ->
      ( b &&  elem f ft , filter (f /= ) ft)) (True,suf) 

test6 = withConnect (\ conn -> do
  addChunkData conn chnkID 
  addSurface conn (chnkID,4,[STop]) 
  addSurface conn (chnkID,5,[SLeft]) 
  addSurface conn (chnkID,6,[SBottom]) 
  fs <- getChunkSurface conn chnkID
  print fs
  return $ fst $ chk fs)
  where
    chnkID = (0,1,0) 
    sufs = [(4,[STop]),(5,[SLeft]),(6,[SBottom])]
    chk = foldr (\ f (b,ft) ->
      ( b &&  elem f ft , filter (f /= ) ft)) (True,sufs) 

test7 = withConnect (\ conn -> do
  addChunkData conn chnkID 
  setChunkSurface conn chnkID sufs
  fs <- getChunkSurface conn chnkID
  return $ fst $ chk fs)
  where
    chnkID = (0,1,0) 
    sufs = [(4,[STop]),(5,[SLeft]),(6,[SBottom])]
    chk = foldr (\ f (b,ft) ->
      ( b &&  elem f ft , filter (f /= ) ft)) (True,sufs) 
-}
specAB :: Spec
specAB = do
  describe "DB test A" $ do
    it "A1: Chunk Check" $
      test1 `shouldReturn` True 
{-    it "A2: Block Check" $
      test2 `shouldReturn` True 
    it "A2-1" $
      test2' `shouldReturn` True 
    it "A2-2" $
      test2'' `shouldReturn` True 
    it "A3" $
      test3 `shouldReturn` True 
    it "A4" $
      test4 `shouldReturn` True 
    it "A5" $
      test5 `shouldReturn` True 
    it "A6" $
      test6 `shouldReturn` True 
    it "A7" $
      test7 `shouldReturn` True 
-}
{-
  describe "DB test B" $ do
    it "B1" $
      wIndexToChunkpos (0,0,0) `shouldBe` ((0,0,0),0)
    it "B2" $
      wIndexToChunkpos (15,15,15) `shouldBe` ((0,0,0),16*16*15+16*15+15)
    it "B3" $
      wIndexToChunkpos (16,0,0) `shouldBe` ((1,0,0),0)
    it "B4" $
      wIndexToChunkpos (16,16,0) `shouldBe` ((1,1,0),0)
    it "B5" $
      wIndexToChunkpos (16,16,16) `shouldBe` ((1,1,1),0)
    it "B6" $
      wIndexToChunkpos (16,17,16) `shouldBe` ((1,1,1),16*16*1)
    it "B7" $
      wIndexToChunkpos (-1,0,0) `shouldBe` ((-1,0,0),15)
    it "B8" $
      ( chunkposToWindex $ wIndexToChunkpos (-1,0,0)) `shouldBe` (-1,0,0)
    it "B9" $
      ( chunkposToWindex $ wIndexToChunkpos (16,0,0)) `shouldBe` (16,0,0)
    it "B10" $
      ( chunkposToWindex $ wIndexToChunkpos (16,16,0)) `shouldBe` (16,16,0)
    it "B11" $
      ( chunkposToWindex $ wIndexToChunkpos (16,0,16)) `shouldBe` (16,0,16)
    it "B12" $
      ( chunkposToWindex $ wIndexToChunkpos (0,16,0)) `shouldBe` (0,16,0)

test8 = do
  hdl <- initDB []
  exitDB hdl
  return True

test9 = do
  hdl <- initDB []
  setBlockToDB hdl (0,0,0) 1
  c <- readChunkData hdl (0,0,0)
  exitDB hdl
  return $ case c of
             (((0,0,0),1):_) -> True
             _ -> False

test10 = do
  hdl <- initDB []
  setBlockToDB hdl (0,0,1) 1
  c1 <- readChunkData hdl (0,0,0)
  delBlockInDB hdl (0,0,1)
  c2 <- readChunkData hdl (0,0,0)
  exitDB hdl
  return $ case c1 of
             (((0,0,1),1):_) -> c2 == []
             _ -> False

test11 = do
  hdl <- initDB []
  writeChunkData hdl (0,0,0) dat
  c <- readChunkData hdl (0,0,0)
  exitDB hdl
  return $ if null c 
             then False
             else fst $ chk c 
  where
    dat = zip [(i,j,k) | i <- [0..15], j <- [0..15], k <-[0..15]] [1..]
    chk = foldr (\ v (b,dt) ->
      ( b &&  elem v dt , filter (v /= ) dt)) (True,dat) 

test12 = do
  hdl <- initDB []
  writeChunkData hdl (1,0,0) dat
  c <- readChunkData hdl (1,0,0)
  exitDB hdl
  return $ if null c 
             then False
             else fst $ chk c 
  where
    dat = zip [(i,j,k) | i <- [0..15], j <- [0..15], k <-[0..15]] [1..]
    rdat = map (\ ((i,j,k),v) -> ((i + 16, j,k),v)) dat 
    chk = foldr (\ v (b,dt) ->
      ( b &&  elem v dt , filter (v /= ) dt)) (True,rdat) 

specCD :: Spec
specCD = do
  describe "DB test C" $ do
    it "C1" $
      test8 `shouldReturn` True
    it "C2" $
      test9 `shouldReturn` True
    it "C3" $
      test10 `shouldReturn` True
    it "C4" $
      test11 `shouldReturn` True
    it "C5" $
      test12 `shouldReturn` True

-}




