module WithSqlite3Spec where

import Test.Hspec

import Hinecraft.DB.WithSqlite3

main :: IO()
main = hspec spec

spec :: Spec
spec = do
  describe "DB test A" $ do
    it "test1" $
      dbfunc1 `shouldBe` 1
    it "test2" $
      dbfunc2 `shouldReturn` True 
    it "test3" $
      dbfunc1 `shouldBe` 1

