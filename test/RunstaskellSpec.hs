{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RunstaskellSpec where

import           Control.Exception
import           System.Exit
import           System.FilePath
import           System.IO
import           System.IO.Silently
import           System.IO.Temp
import           Test.Hspec

import           BootstrapSpec
import           PackageSets
import           Path
import           Runstaskell
import           Sandboxes

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

  describe "runScript" $ do
    let stdoutCode = unlines $
          "import System.Exit" :
          "import System.IO" :
          "main = do" :
          "  hPutStrLn stdout $ \"this goes to stdout\"" :
          []

        stderrCode = unlines $
          "import System.Exit" :
          "import System.IO" :
          "main = do" :
          "  hPutStrLn stderr $ \"this goes to stderr\"" :
          "  exitWith $ ExitFailure 23" :
          []

        argsCode = unlines $
          "import System.Environment" :
          "main = getArgs >>= print" :
          []

    it "inherits stdout/stderr and exitcode from running the script" $ do
      withBootstrappedScript stdoutCode $ \executable sandboxes scriptPath -> do
        output <- hCapture_ [stdout] $
          runScript executable sandboxes scriptPath []
        output `shouldBe` "this goes to stdout\n"

    it "inherits stderr and exitcode from running the script" $ do
      withBootstrappedScript stderrCode $ \executable sandboxes scriptPath -> do
        output <- hCapture_ [stderr] $
          runScript executable sandboxes scriptPath []
            `catch` \e ->  e `shouldBe` ExitFailure 23
        output `shouldBe` "this goes to stderr\n"

    it "passes arguments to the running script" $ do
      withBootstrappedScript argsCode $ \executable sandboxes scriptPath -> do
        let args = ["arg1", "arg2"]
        output <- hCapture_ [stdout] $
          runScript executable sandboxes scriptPath args
        output `shouldBe` (show args ++ "\n")

withBootstrappedScript :: String
                       -> (Path ProgName -> Path Sandboxes -> Path Script -> IO ())
                       -> IO ()
withBootstrappedScript code action =
  withBootstrapped "test" $ \ binPath dataPath ->
    withSystemTempFile "Code.hs" $ \ scriptPath handle -> do
      hPutStr handle code
      hClose handle
      action
        (Path $ toPath binPath </> "runstaskell-test")
        (getSandboxes dataPath)
        (Path scriptPath)
