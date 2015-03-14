{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}

module Runstaskell where

import           Data.List
import           Data.String
import           System.Environment  (getArgs, setEnv)
import           System.FilePath
import           System.Process

import           Main.Utils
import           PackageSets
import           Path

run :: IO ()
run = do
  progName <- getProgName
  sandboxes <- getSandboxes
  args <- getArgs
  runstaskell progName sandboxes args

runstaskell :: Path ProgName -> Path Sandboxes -> [String] -> IO ()
runstaskell progName sandboxes args = do
  let packageSetName = getPackageNameSetFromProgName progName
      sandbox = getSandbox sandboxes packageSetName
      sandboxConfig = getCabalSandboxConfig sandbox
  setEnv "CABAL_SANDBOX_CONFIG" (toPath sandboxConfig)
  case args of
    ["--list"] -> callCommand "cabal exec -- ghc-pkg list"
    (script : args) -> callCommand
      ("cabal exec -- runhaskell " ++ script ++ " " ++ unwords args)
    _ -> undefined

getPackageNameSetFromProgName :: Path ProgName -> PackageSetName
getPackageNameSetFromProgName (Path (takeFileName -> p))
  | "runstaskell-" `isPrefixOf` p =
    fromString $ drop (length "runstaskell-") p
getPackageNameSetFromProgName _ = latest
