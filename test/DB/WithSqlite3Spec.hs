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
  specCD

withConnect fn = do
  tmp <- newChan 
  conn <- initProcess ":memory:"
  --conn <- initProcess "tmp.db"
  ret <- fn conn
  exitProcess tmp conn 
  return ret

test1 = withConnect (\ conn -> do
  chk1 <- checkChunk conn chnkID 
  addChunk conn chnkID 
  Just cid <- _getChunkID conn chnkID
  chk2 <- checkChunk conn chnkID 
  mapM_ (\ j -> addBlockToChunk conn (chnkID,j)) [0..9]
  return $ and [not chk1, chk2, cid == 1])
  where
    chnkID = (0,0) 

test2 = withConnect (\ conn -> do
  addChunk conn chnkID 
  mapM_ (\ j -> addBlockToChunk conn (chnkID,j)) [0..9]

  p1 <- getCellObject conn (chnkID,2,4)
  addCellObject conn (chnkID,2,4,5)
  p2 <- getCellObject conn (chnkID,2,4)
  flg <- return $ case p2 of
    Just v -> v == 5
    Nothing -> False 
  return $ and [p1 == Nothing, flg])
  where
    chnkID = (0,1) 

test3 = withConnect (\ conn -> do
  addChunk conn chnkID 
  mapM_ (\ j -> addBlockToChunk conn (chnkID,j)) [0..9]

  setChunkBlock conn chnkID
    $ [(3, zip [1..1000] [11 .. ])]
  Just p1 <- getCellObject conn (chnkID,3,1)
  Just p2 <- getCellObject conn (chnkID,3,200)
  Just p3 <- getCellObject conn (chnkID,3,201)
  Just p4 <- getCellObject conn (chnkID,3,1000)
  return $ and [p1 == 11, p2 == 210, p3 == 211, p4 == 1010])
  where
    chnkID = (2,0) 

test4 = withConnect (\ conn -> do
  addChunk conn chnkID 
  mapM_ (\ j -> addBlockToChunk conn (chnkID,j)) [0..9]
  setChunkBlock conn chnkID [(2,dat)]
  blst <- getChunkBlock conn chnkID
  return $ and $ map (\ (x,y) -> x == y ) $ zip dat ( blst !! 2))
  where
    chnkID = (0,-1) 
    dat = zip [1..1000] [11 .. ]

test5 = withConnect (\ conn -> do
  addChunk conn chnkID 
  mapM_ (\ j -> addBlockToChunk conn (chnkID,j)) [0..9]
  addCellObject conn (chnkID,1,4,5)
  updateCellObject conn (chnkID,1,4,2)
  p1 <- getCellObject conn (chnkID,1,4)
  flg <- return $ case p1 of
    Just v -> v == 2
    Nothing -> False 
  return flg)
  where
    chnkID = (0,3) 

test6 = withConnect (\ conn -> do
  addChunk conn chnkID 
  mapM_ (\ j -> addBlockToChunk conn (chnkID,j)) [0..9]
  addCellObject conn (chnkID,3,4,5)
  Just p1 <- getCellObject conn (chnkID,3,4)
  deleteCellObject conn (chnkID,3,4)
  p2 <- getCellObject conn (chnkID,3,4)
  flg <- return $ case p2 of
    Just v -> False
    Nothing -> True 
  return $ and [p1 == 5, flg])
  where
    chnkID = (-5,0) 

test7 = withConnect (\ conn -> do
  addChunk conn chnkID 
  mapM_ (\ j -> addBlockToChunk conn (chnkID,j)) [0..9]
  addSurface conn (chnkID,2,4,[STop]) 
  f1 <- getSurface conn (chnkID,2,4)
  flg1 <- return $ case f1 of
    [STop] -> True
    _ -> False 

  updateSurface conn (chnkID,2,4,suf) 
  f2 <- getSurface conn (chnkID,2,4)
  flg2 <- return $ fst $ chk f2

  deleteSurface conn (chnkID,2,4) 
  f3 <- getSurface conn (chnkID,2,4)
  flg3 <- return $ f3 == []

  return $ and [flg1, flg2, flg3])
  where
    chnkID = (3,0) 
    suf = [SLeft,SRight]
    chk = foldr (\ f (b,ft) ->
      ( b &&  elem f ft , filter (f /= ) ft)) (True,suf) 

test8 = withConnect (\ conn -> do
  addChunk conn chnkID 
  mapM_ (\ j -> addBlockToChunk conn (chnkID,j)) [0..9]
  addSurface conn (chnkID,5,4,[STop]) 
  addSurface conn (chnkID,5,5,[SLeft]) 
  addSurface conn (chnkID,5,6,[SBottom]) 
  fs' <- getChunkSurface conn chnkID
  print fs'
  let fs = fs' !! 5
  return $ fst $ chk fs)
  where
    chnkID = (0,1) 
    sufs = [(4,[STop]),(5,[SLeft]),(6,[SBottom])]
    chk = foldr (\ f (b,ft) ->
      ( b &&  elem f ft , filter (f /= ) ft)) (True,sufs) 

test9 = withConnect (\ conn -> do
  addChunk conn chnkID 
  mapM_ (\ j -> addBlockToChunk conn (chnkID,j)) [0..9]
  setChunkSurface conn chnkID [(1,sufs)]
  fs' <- getChunkSurface conn chnkID
  let fs = fs' !! 1
  return $ fst $ chk fs)
  where
    chnkID = (0,7) 
    sufs = [(4,[STop]),(5,[SLeft]),(6,[SBottom])]
    chk = foldr (\ f (b,ft) ->
      ( b &&  elem f ft , filter (f /= ) ft)) (True,sufs) 

specAB :: Spec
specAB = do
  describe "DB test A" $ do
    it "A1: Chunk Check" $
      test1 `shouldReturn` True 
    it "A2: Block Check" $
      test2 `shouldReturn` True 
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
    it "A8" $
      test8 `shouldReturn` True
    it "A9" $
      test9 `shouldReturn` True

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

testC1 = do
  hdl <- initDB []
  exitDB hdl
  return True

testC2 = do
  hdl <- initDB []
  setBlockToDB hdl (0,0,0) 1 []
  r@(c:_) <- readChunkData hdl (0,0)
  print $ show r
  exitDB hdl
  return $ case c of
             (((0,0,0),1):_) -> True
             _ -> False

testC3 = do
  hdl <- initDB []
  setBlockToDB hdl (0,0,1) 1 []
  (c1:_) <- readChunkData hdl (0,0)
  delBlockInDB hdl (0,0,1)
  (c2:_) <- readChunkData hdl (0,0)
  exitDB hdl
  return $ case c1 of
             (((0,0,1),1):_) -> c2 == []
             _ -> False

testC4 = do
  hdl <- initDB []
  writeChunkData hdl (0,0) $ dat : (replicate (bsize -1) [])
  (c:_) <- readChunkData hdl (0,0)
  exitDB hdl
  return $ if null c 
             then False
             else fst $ chk c 
  where
    bsize = blockSize chunkParam
    dat = zip [(i,j,k) | i <- [0..15], j <- [0..15], k <-[0..15]] [1..]
    chk = foldr (\ v (b,dt) ->
      ( b &&  elem v dt , filter (v /= ) dt)) (True,dat) 

testC5 = do
  hdl <- initDB []
  writeChunkData hdl (1,0) $ dat : (replicate (bsize -1) [])
  (c:_) <- readChunkData hdl (1,0)
  exitDB hdl
  return $ if null c 
             then False
             else fst $ chk c 
  where
    bsize = blockSize chunkParam
    dat = zip [(i,j,k) | i <- [0..15], j <- [0..15], k <-[0..15]] [1..]
    rdat = map (\ ((i,j,k),v) -> ((i + 16, j,k),v)) dat 
    chk = foldr (\ v (b,dt) ->
      ( b &&  elem v dt , filter (v /= ) dt)) (True,rdat) 

specCD :: Spec
specCD = do
  describe "DB test C" $ do
    it "C1" $
      testC1 `shouldReturn` True
    it "C2" $
      testC2 `shouldReturn` True
    it "C3" $
      testC3 `shouldReturn` True
    it "C4" $
      testC4 `shouldReturn` True
    it "C5" $
      testC5 `shouldReturn` True


