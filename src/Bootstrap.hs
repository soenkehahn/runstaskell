{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bootstrap (run, runBootstrap) where

import           Data.Map                       (keys)
import           Generics.SOP
import qualified GHC.Generics
import           System.Console.GetOpt.Generics
import           System.Directory
import           System.Environment
import           System.Exit.Compat
import           System.FilePath
import           System.Posix.Files
import           System.Process

import           Main.Utils
import           PackageSets
import           Path

data Options
  = Options {
    bootstrap :: Maybe String,
    list :: Bool
  }
  deriving (GHC.Generics.Generic)

instance Generic Options
instance HasDatatypeInfo Options

run :: IO ()
run = withArguments $ \ options -> case options of
  (Options{list = True}) -> listPackageSets
  (Options{bootstrap = Just packageSetName}) -> do
    binDir :: Path Bin <- getBinDir
    sandboxes <- getSandboxes
    runBootstrap binDir sandboxes (PackageSetName packageSetName)
  _ -> die "missing option: --bootstrap=string"

listPackageSets :: IO ()
listPackageSets = mapM_ putStrLn $ map fromPackageSetName $ keys packageSets

runBootstrap :: Path Bin -> Path Sandboxes -> PackageSetName -> IO ()
runBootstrap binDir sandboxesDir packageSetName = do
  packageSet <- either die return (getPackageSet packageSetName)
  let sandboxDir = getSandbox sandboxesDir packageSetName
  createDirectoryIfMissing True (toPath sandboxDir)
  setCurrentDirectory (toPath sandboxDir)
  callCommand "cabal sandbox init"
  writeCabalConfig sandboxDir packageSet
  setEnv "CABAL_SANDBOX_CONFIG" (toPath $ getCabalSandboxConfig sandboxDir)
  callCommand ("cabal install " ++ unwords (packageNames packageSet))
  createSymbolicLink
    (toPath binDir </> "runstaskell")
    (toPath $ mkProgramLink binDir packageSetName)
