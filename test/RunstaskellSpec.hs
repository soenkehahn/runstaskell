{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module RunstaskellSpec where

import           System.FilePath
import           System.IO.Silently
import           Test.Hspec

import           BootstrapSpec
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

  describe "runstaskell" $ do
    it "uses the test packages when called with runstaskell-test" $
      withBootstrapped "test" $ \ prefix -> do
        let progName :: Path ProgName = Path "runstaskell-test"
            sandboxes :: Path Sandboxes = Path (prefix </> "sandboxes")
        output <- capture_ $ runstaskell progName sandboxes ["--list"]
        output `shouldContain` "tagged-0.7"
