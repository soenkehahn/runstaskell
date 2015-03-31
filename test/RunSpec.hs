{-# LANGUAGE OverloadedStrings #-}

module RunSpec where

import           System.IO.Silently
import           Test.Hspec
import           Test.QuickCheck

import           BootstrapSpec
import           Path
import           Run

spec :: Spec
spec = do
  describe "parseOptions" $ do

    it "supports --help" $ do
      let (Help message) = parseOptions (Path "prog") ["--help"]
      message `shouldContain` "--bootstrap PACKAGE_SET"

    it "supports --list-available" $ do
      parseOptions (Path "prog") ["--list-installable"] `shouldBe` ListInstallable

    it "supports --list-bootstrapped" $ do
      parseOptions (Path "prog") ["--list-bootstrapped"] `shouldBe` ListBootstrapped

    it "supports ./script.hs" $ do
      parseOptions (Path "prog") ["./script.hs"] `shouldBe` RunScript (Path "./script.hs") []

    it "supports arbitrary arguments for scripts" $ do
      property $ \ args -> parseOptions (Path "prog") ("./script.hs" : args) `shouldBe`
        RunScript (Path "./script.hs") args

    it "complains about invalid options" $ do
      parseOptions (Path "prog") ["--invalid", "bla"] `shouldBe`
        Error "invalid option: --invalid"

  describe "runListAvailable" $ do
    it "lists available package sets" $ do
      output <- capture_ runListInstallable
      lines output `shouldContain` ["test"]
      lines output `shouldContain` ["rc-1.14"]

  describe "runListBootstrapped" $ do
    it "lists bootstrapped packages" $ withBootstrapped "test" $ \ _ dataDir -> do
      output <- capture_ $ runListBootstrapped dataDir
      lines output `shouldBe` ["test"]
