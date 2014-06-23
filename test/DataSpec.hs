{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE BangPatterns #-}

module DataSpec where

import Test.Hspec
import qualified Data.Map as M

--import Hinecraft.Data
import Hinecraft.Types

{-
main :: IO()
main = hspec spec

spec :: Spec
spec = do
-}

(bx,bz) = (0,0)
bplst = [0,1]
sufList = M.fromList
  [((0,0),[M.empty,M.fromList (zip [0 .. 5] [3..]),M.empty])]

slst = M.toList $ M.filterWithKey
              (\ (i,j) v -> (bx + 1) > i && i > (bx - 1)
                         && (bz + 1) > j && j > (bz - 1)) sufList
f = map (\ (key,v) -> (key, filter (\ (i,_) -> elem i bplst)
                                      $ zip [0..] v)) slst
tf = concatMap (\ (key,m) -> map (\ (j,v) -> (key,j, M.toList v)) m) f 
-- [((0,0),0,[]),((0,0),1,[(0,3),(1,4),(2,5),(3,6),(4,7),(5,8)])]

f' = concatMap (\ (cidx,j,v) -> map (\ (p,bid) -> (chunkposToWindex (cidx,j,p),bid)) v ) tf 

{-
f' = map (\ (cidx,j,v) -> (chunkposToWindex cidx j   ,v) 
           $ concatMap (\ (key,m) -> map ((j,v) -> (key,j, M.toList v)) m) f 
f'' = filter chkArea (concat f')
res = filter chkJustAndFront
-}
-- #####################
chunkposToWindex :: (ChunkIdx,Int,Int) -> WorldIndex
chunkposToWindex ((i,k),j,idx) = (x,y,z)
  where
    bsize = blockSize chunkParam
    x = bsize * i + lx
    y = bsize * j + ly
    z = bsize * k + lz
    (ly,t) = idx `divMod` (bsize * bsize)
    (lz,lx) = t `divMod` bsize
