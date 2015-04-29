{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module PackageSets where

import           Control.Applicative
import           Data.List
import           Data.Map                  hiding (filter, map)
import           Data.String
import           Data.Traversable
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           System.Directory
import           System.FilePath

import           PackageSets.Types
import           Path

-- * package set definitions

getPackageSet :: PackageSetName -> Either String PackageSet
getPackageSet name = maybe
  (Left ("unknown package set: " ++ fromPackageSetName name))
  Right
  (Data.Map.lookup name packageSets)

packageSets :: Map PackageSetName PackageSet
packageSets = fromList $
  ("test", PackageSet [("tagged", "0.7")]) :
  map
    (\ version ->
      (fromString ("rc-" ++ showVersion version),
       StackageConfigFile
         ("http://www.stackage.org/snapshot/lts-" ++ showVersion version ++ "/cabal.config")
         stackagePackages))
    ltsVersions ++
  customPackageSets ++
  []

latest :: PackageSetName
latest = fromString ("rc-" ++ showVersion (maximum ltsVersions))

showVersion :: (Int, Int) -> String
showVersion (a, b) = show a ++ "." ++ show b

ltsVersions :: [(Int, Int)]
ltsVersions =
  map (1,) [1 .. 15] ++
  map (2,) [1 .. 5]

stackagePackages :: [String]
stackagePackages =
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
  "silently" :
  "string-conversions" :
  "tagged" :
  "temporary" :
  "transformers" :
  "yaml" :
  "hspec" :
  []

-- * custom package sets

customPackageSets :: [(PackageSetName, PackageSet)]
customPackageSets = $(do
  let customDir = "customPackageSets"
  files <-
    filter (not . ("." `isPrefixOf`)) <$>
    runIO (getDirectoryContents customDir)
  packageSets <- forM files $ \ file -> do
    content <- runIO (readFile (customDir </> file))
    return (PackageSetName file, CustomPackageSet content)
  dataToExpQ (const Nothing) packageSets)
