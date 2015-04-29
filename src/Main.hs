
module Main where

import           Control.Applicative
import           Control.Monad
import qualified System.Environment

import           Path
import           Run

import qualified Paths_runstaskell

getBinDir :: IO (Path Bin)
getBinDir = Path <$> Paths_runstaskell.getBinDir

getProgName :: IO (Path ProgName)
getProgName = Path <$> System.Environment.getProgName

getDataDir :: IO (Path DataDir)
getDataDir = Path <$> Paths_runstaskell.getDataDir

main :: IO ()
main = join (run <$> getProgName <*> getBinDir <*> getDataDir)
