#!/usr/bin/env runstaskell

import Test.Hspec

main :: IO ()
main = hspec $
  describe "Prelude.head" $ do
    it "returns head of a list" $ do
      head [23..] `shouldBe` (23 :: Int)
