{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns     #-}

module PackageSets.Types where

import           Data.Data
import           Data.List
import           Data.Maybe
import           System.Process

import           Path

data PackageSet
  = PackageSet {
    cabalConfig :: [(String, String)]
  }
  | StackageConfigFile {
    cabalConfigUrl :: String,
    packages :: [String]
  }
  | CustomPackageSet {
    cabalConfigFile :: String
  }
  deriving (Show, Eq, Typeable, Data)

packageNames :: PackageSet -> [String]
packageNames PackageSet{cabalConfig} = map fst cabalConfig
packageNames (StackageConfigFile _ names) = names
packageNames (CustomPackageSet cabalConfig)
  | Just comment <- listToMaybe (lines cabalConfig)
  , "--" `isPrefixOf` comment
  = words (drop 2 comment)
packageNames (CustomPackageSet _) = []

writeCabalConfig :: Path Sandbox -> PackageSet -> IO ()
writeCabalConfig sandboxDir (PackageSet cabalConfig) = do
  writeFile (toPath $ getCabalConfig sandboxDir) $ unlines $
    "constraints:" :
    map (\ (package, version) -> "  " ++ package ++ " == " ++ version)
        cabalConfig
writeCabalConfig sandboxDir (StackageConfigFile url _) = do
  callCommand ("wget '" ++ url ++ "' -O " ++ toPath (getCabalConfig sandboxDir))
writeCabalConfig sandboxDir (CustomPackageSet content) = do
  writeFile (toPath $ getCabalConfig sandboxDir) content
