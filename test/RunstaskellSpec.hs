{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RunstaskellSpec where

import           Test.Hspec

import           PackageSets
import           Path
import           Runstaskell

spec :: Spec
spec = do
  describe "getPackageNameSetFromProgramPath" $ do
    it "parses runstaskell-test" $ do
      getPackageNameSetFromProgName (Path "runstaskell-test")
        `shouldBe` "test"

    it "parses runstaskell-1.11" $ do
      getPackageNameSetFromProgName (Path "runstaskell-1.11")
        `shouldBe` "1.11"

    it "returns the default for runstaskell" $ do
      getPackageNameSetFromProgName (Path "runstaskell")
        `shouldBe` latest

    it "returns the default for foo" $ do
      getPackageNameSetFromProgName (Path "foo")
        `shouldBe` latest
