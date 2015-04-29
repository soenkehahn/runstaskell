
module Sandboxes where

import           Control.Applicative
import           System.Directory
import           System.FilePath

import           Path

getBootstrappedSandboxes :: Path Sandboxes -> IO [Path Sandbox]
getBootstrappedSandboxes sandboxesDir =
  map (Path :: FilePath -> Path Sandbox) <$>
  map (toPath sandboxesDir </>) <$>
  filter (not . (`elem` [".", ".."])) <$>
  getDirectoryContents (toPath sandboxesDir)


getSandboxes :: Path DataDir -> Path Sandboxes
getSandboxes dataDir = Path $ (toPath dataDir </> "sandboxes")

getSandbox :: Path Sandboxes -> PackageSetName -> Path Sandbox
getSandbox (Path sandboxesDir) (PackageSetName name) =
  Path (sandboxesDir </> name)
