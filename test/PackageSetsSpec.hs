
module PackageSetsSpec where

import           Control.Applicative
import           Data.List
import           Data.Traversable
import           System.Directory
import           System.FilePath
import           Test.Hspec

import           PackageSets
import           PackageSets.Types
import           Path

spec :: Spec
spec = do
  describe "customPackageSets" $ do
    it "reads custom package sets from disk" $ do
      customFiles <- do
        files <-
          sort <$>
          filter (not . ("." `isPrefixOf`)) <$>
          getDirectoryContents "customPackageSets"
        forM files $ \ file -> do
          cabalConfig <- readFile ("customPackageSets" </> file)
          return (PackageSetName file, CustomPackageSet cabalConfig)
      customPackageSets `shouldBe` customFiles
