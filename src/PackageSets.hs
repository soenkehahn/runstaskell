{-# LANGUAGE  OverloadedStrings #-}

module PackageSets where

import           Data.Map        hiding (map)

import           Path

data PackageSet
  = PackageSet {
    cabalConfig :: [(String, String)]
   }

packageNames :: PackageSet -> [String]
packageNames (PackageSet cabalConfig) = map fst cabalConfig

writeCabalConfig :: Path Sandbox -> PackageSet -> IO ()
writeCabalConfig sandboxDir (PackageSet cabalConfig) = do
  writeFile (toPath $ getCabalConfig sandboxDir) $ unlines $
    "constraints:" :
    map (\ (package, version) -> "  " ++ package ++ " == " ++ version)
        cabalConfig


-- * package set definitions

getPackageSet :: PackageSetName -> PackageSet
getPackageSet "test" = packageSets ! "test"
getPackageSet _ = undefined

packageSets :: Map PackageSetName PackageSet
packageSets = fromList $
  ("test", PackageSet [("tagged", "0.7")]) :
  []

latest :: PackageSetName
latest = "test"

{-
packages :: [String]
packages =
  "aeson" :
  "base-compat" :
  "case-insensitive" :
  "cassava" :
  "cmdArgs" :
  "containers" :
  "deepseq" :
  "directory" :
  "directory-tree" :
  "either" :
  "enclosed-exceptions" :
  "errors" :
  "filepath" :
  "http-client" :
  "mime-mail" :
  "mtl" :
  "optparse-applicative" :
  "process" :
  "safe" :
  "shell-conduit" :
  "shelly" :
  "silently" :
  "string-conversions" :
  "tagged" :
  "temporary" :
  "transformers" :
  "yaml" :
  []
-}
