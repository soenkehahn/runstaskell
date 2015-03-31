{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import           Sandboxes

spec :: Spec
spec = do
  describe "bootstrap" $ do
    it "bootstraps the test package set" $
      withBootstrapped "test" $ \ _ dataDir -> do
        cabalConfig <- readFile (toPath (getSandboxes dataDir) </> "test" </> "cabal.config")
        cabalConfig `shouldContain` "tagged == 0.7"
        setCurrentDirectory (toPath (getSandboxes dataDir) </> "test")
        output <- capture_ $ callCommand "cabal exec ghc-pkg list"
        output `shouldContain` "tagged-0.7"

    it "throws an exception on unknown package sets" $ do
      (withBootstrapped "foo" $ \ _ _ -> return ())
        `shouldThrow` anyException

    it "creates a link in bindir" $ do
      withBootstrapped "test" $ \ (binDir :: Path Bin) _ -> do
        readSymbolicLink (toPath binDir </> "runstaskell-test")
          `shouldReturn` (toPath binDir </> "runstaskell")

withBootstrapped :: PackageSetName -> (Path Bin -> Path Data -> IO ()) -> IO ()
withBootstrapped packageSetName action = do
  withSystemTempDirectory "runstaskell-test" $ \ prefix ->
    protectCurrentDirectory $ do
      let binDir :: Path Bin = Path (prefix </> "bin")
          dataDir :: Path Data = Path (prefix </> "data")
      mapM_ unsetEnv $
        "CABAL_SANDBOX_CONFIG" :
        "CABAL_SANDBOX_PACKAGE_PATH" :
        "GHC_PACKAGE_PATH" :
        []
      createDirectoryIfMissing True (prefix </> "bin")
      writeFile (prefix </> "bin" </> "runstaskell") ""
      runBootstrap
        (Path (prefix </> "bin") :: Path Bin)
        (getSandboxes dataDir)
        packageSetName
      action binDir dataDir

protectCurrentDirectory :: IO a -> IO a
protectCurrentDirectory =
  bracket getCurrentDirectory setCurrentDirectory . const
