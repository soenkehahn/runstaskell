{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Runstaskell where

import           Control.Monad
import           Data.List
import           Data.String
import           System.Environment (setEnv)
import           System.Exit
import           System.FilePath
import           System.Process

import           PackageSets
import           Path
import           Sandboxes

{- run :: IO ()
run = do
  progName <- getProgName
  sandboxes <- getSandboxes
  args <- getArgs
  runstaskell progName sandboxes args -}

runScript :: Path ProgName -> Path Sandboxes -> Path Script -> [String] -> IO ()
runScript progName sandboxes script args = do
  let packageSetName = getPackageNameSetFromProgName progName
      sandbox = getSandbox sandboxes packageSetName
      sandboxConfig = getCabalSandboxConfig sandbox
      command = "cabal exec -- runhaskell " ++ toPath script ++ " " ++ unwords args
  setEnv "CABAL_SANDBOX_CONFIG" (toPath sandboxConfig)
  exitCode <- system command
  unless (exitCode == ExitSuccess) $ exitWith exitCode

getPackageNameSetFromProgName :: Path ProgName -> PackageSetName
getPackageNameSetFromProgName (Path (takeFileName -> p))
  | "runstaskell-" `isPrefixOf` p =
    fromString $ drop (length "runstaskell-") p
getPackageNameSetFromProgName _ = latest
