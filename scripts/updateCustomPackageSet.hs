#!/usr/bin/env runstaskell-custom-1

import Prelude ()
import Prelude.Compat

import Control.Exception
import Control.Monad.Compat
import Data.List.Compat
import System.Environment.Compat
import System.IO
import System.IO.Silently
import System.Process
import System.Exit.Compat
import Test.Mockery.Directory

import PackageSets.Types

customPackages :: [String]
customPackages =
  "aeson" :
  "base-compat" :
  "base-orphans" :
  "containers" :
  "directory" :
  "enclosed-exceptions" :
  "filepath" :
  "getopt-generics" :
  "hspec" :
  "hspec-contrib" :
  "mockery" :
  "process" :
  "safe" :
  "silently" :
  "string-conversions" :
  "temporary" :
  "yaml" :
  []

main :: IO ()
main = do
  output <- inTempDirectory $ silence $ do
    mapM_ unsetEnv (words "CABAL_SANDBOX_CONFIG CABAL_SANDBOX_PACKAGE_PATH GHC_PACKAGE_PATH")
    callCommand "cabal sandbox init"
    writeFile "foo.cabal" ("\
      \library\n\
      \  build-depends: " ++
        intercalate ", " customPackages ++ "\n")
    callCommand "cabal freeze"
    callCommand "cat cabal.config"
    content <- readFile "cabal.config"
    return ("-- " ++ unwords customPackages ++ "\n" ++ content)
  let parsedPackageNames = packageNames (CustomPackageSet output)
  unless (customPackages `isSubsetOf` parsedPackageNames) $ do
    die $ unlines $
      "parsing of package names from cabal.config failed:" :
      show parsedPackageNames :
      "-- should be a superset of --" :
      show customPackages :
      []
  when (null parsedPackageNames) $
    die $ "parsing of package names returned no results"
  putStr output

isSubsetOf :: Eq a => [a] -> [a] -> Bool
isSubsetOf a b = all (\ x -> x `elem` b) a
