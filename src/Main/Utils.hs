
module Main.Utils where

import           Control.Applicative
import           System.Environment
import           System.FilePath

import           Path

import qualified Paths_runstaskell

getSandboxes :: IO (Path Sandboxes)
getSandboxes = Path <$> (</> "sandboxes") <$> Paths_runstaskell.getDataDir

getBinDir :: IO (Path Bin)
getBinDir = Path <$> Paths_runstaskell.getBinDir

getProgName :: IO (Path ProgName)
getProgName = Path <$> System.Environment.getProgName
