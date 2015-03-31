{-# LANGUAGE OverloadedStrings #-}

module BootstrapSpec where

import           Control.Exception
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO.Silently
import           System.IO.Temp
import           System.Posix.Files
import           System.Process
import           Test.Hspec

import           Bootstrap
import           Path

spec :: Spec
spec = do
  describe "bootstrap" $ do
    it "bootstraps the test package set" $
      withBootstrapped "test" $ \ prefix -> do
        cabalConfig <- readFile (prefix </> "sandboxes" </> "test" </> "cabal.config")
        cabalConfig `shouldContain` "tagged == 0.7"
        setCurrentDirectory (prefix </> "sandboxes" </> "test")
        output <- capture_ $ callCommand "cabal exec ghc-pkg list"
        output `shouldContain` "tagged-0.7"

    it "throws an exception on unknown package sets" $ do
      (withBootstrapped "foo" $ const $ return ())
        `shouldThrow` anyException

    it "creates a link in bindir" $ do
      withBootstrapped "test" $ \ prefix -> do
        readSymbolicLink (prefix </> "bin" </> "runstaskell-test")
          `shouldReturn` (prefix </> "bin" </> "runstaskell")

  describe "run" $ do

    it "supports --help" $ do
      output <- capture_ $ withArgs ["--help"] run
      output `shouldContain` "--bootstrap="

    it "supports --list" $ do
      output <- capture_ $ withArgs ["--list"] run
      output `shouldContain` "test"
      output `shouldContain` "rc-1.14"

withBootstrapped :: PackageSetName -> (FilePath -> IO ()) -> IO ()
withBootstrapped packageSetName action = do
  withSystemTempDirectory "runstaskell-test" $ \ prefix ->
    protectCurrentDirectory $ do
      mapM_ unsetEnv $
        "CABAL_SANDBOX_CONFIG" :
        "CABAL_SANDBOX_PACKAGE_PATH" :
        "GHC_PACKAGE_PATH" :
        []
      createDirectoryIfMissing True (prefix </> "bin")
      writeFile (prefix </> "bin" </> "runstaskell") ""
      let sandboxes = Path (prefix </> "sandboxes")
      runBootstrap
        (Path (prefix </> "bin") :: Path Bin)
        sandboxes
        packageSetName
      action prefix

protectCurrentDirectory :: IO a -> IO a
protectCurrentDirectory =
  bracket getCurrentDirectory setCurrentDirectory . const
