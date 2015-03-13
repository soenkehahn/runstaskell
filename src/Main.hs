
module Main where

import           System.Environment
import           System.Exit
import           System.FilePath
import           System.Process

import           Paths_runstaskell

main :: IO ()
main = do
  dataDir <- getDataDir
  let sandboxConfig = dataDir </> "sandbox" </> "cabal.sandbox.config"
  setEnv "CABAL_SANDBOX_CONFIG" sandboxConfig
  args <- getArgs
  process <- spawnProcess "cabal" ("exec" : "--" : "runhaskell" : args)
  exitCode <- waitForProcess process
  exitWith exitCode
