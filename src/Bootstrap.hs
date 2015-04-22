{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Bootstrap (runBootstrap) where

import           System.Directory
import           System.Environment
import           System.Exit.Compat
import           System.FilePath
import           System.Posix.Files
import           System.Process

import           PackageSets
import           Path
import           Sandboxes

runBootstrap :: Path Bin -> Path Sandboxes -> PackageSetName -> IO ()
runBootstrap binDir sandboxesDir packageSetName = do
  packageSet <- either die return (getPackageSet packageSetName)
  let sandboxDir = getSandbox sandboxesDir packageSetName
  createDirectoryIfMissing True (toPath sandboxDir)
  setCurrentDirectory (toPath sandboxDir)
  callCommand "cabal sandbox init"
  writeCabalConfig sandboxDir packageSet
  setEnv "CABAL_SANDBOX_CONFIG" (toPath $ getCabalSandboxConfig sandboxDir)
  callCommand ("cabal install --force-reinstalls " ++ unwords (packageNames packageSet))
  createSymbolicLink
    (toPath binDir </> "runstaskell")
    (toPath $ mkProgramLink binDir packageSetName)
