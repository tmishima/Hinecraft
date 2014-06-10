{-# LANGUAGE OverloadedStrings #-} 

module DB.WithSqlite3Spec where

import Test.Hspec
import Database.SQLite.Simple

import Hinecraft.DB.WithSqlite3

main :: IO()
main = hspec spec

withConnect fn = do
  conn <- initProcess ":memory:"
  ret <- fn conn
  exitProcess conn 
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
    chnkID = (0,0,0) 

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

test2' = withConnect (\ conn -> do
  addChunkData conn chnkID 
  setBlocksToChunk conn chnkID
    [ (1,2), (2,3) , (3,4)] 
  Just p1 <- getBlockPos conn (chnkID,1)
  Just p2 <- getBlockPos conn (chnkID,2)
  Just p3 <- getBlockPos conn (chnkID,3)
  return $ and [p1 == 2, p2 == 3, p3 == 4])
  where
    chnkID = (0,0,0) 

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


spec :: Spec
spec = do
  describe "DB test A" $ do
    it "test1: Chunk Check" $
      test1 `shouldReturn` True 
    it "test2: Block Check" $
      test2 `shouldReturn` True 
    it "test2'" $
      test2' `shouldReturn` True 
    it "test3" $
      test3 `shouldReturn` True 
    it "test4" $
      test4 `shouldReturn` True 
    it "test5" $
      test5 `shouldReturn` True 
    it "test6" $
      test6 `shouldReturn` True 
    it "sample" $
      dbfunc1 `shouldBe` 1

