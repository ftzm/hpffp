module Addition where

import Test.Hspec
import Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "hello!"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
         | n < d = (count, n)
         | otherwise =
             go (n - d) d (count + 1)

main1 :: IO ()
main1 = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      2 + 2 `shouldBe` 4

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is\
       \ 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2)

recMult :: (Eq a, Num a) => a -> a -> a
recMult a 0 = 0
recMult 0 b = 0
recMult a b = go a b 0
  where go a 0 c = c
        go a b c = go a (b-1) (c+a)

testRecMult :: IO ()
testRecMult = hspec $ do
  describe "Addition" $ do
    it "first arg is 0 yields 0" $ do
      recMult 0 3 `shouldBe` 0
    it "second arg is 0 yields 0" $ do
      recMult 3 0 `shouldBe` 0
    it "3 times 3 is 9" $ do
      recMult 3 0 `shouldBe` 0

qMain :: IO ()
qMain = hspec $ do
  describe "Addition" $ do
    it "x + 1 is always\
       \than x" $ do
      property $ \x -> x + 1 > (x :: Int)
