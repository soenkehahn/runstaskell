{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Runstaskell where

import           Data.List
import           Data.String
import           System.Environment (setEnv)
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
  setEnv "CABAL_SANDBOX_CONFIG" (toPath sandboxConfig)
  callCommand
    ("cabal exec -- runhaskell " ++ toPath script ++ " " ++ unwords args)

getPackageNameSetFromProgName :: Path ProgName -> PackageSetName
getPackageNameSetFromProgName (Path (takeFileName -> p))
  | "runstaskell-" `isPrefixOf` p =
    fromString $ drop (length "runstaskell-") p
getPackageNameSetFromProgName _ = latest
