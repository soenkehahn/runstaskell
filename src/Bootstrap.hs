{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}

module Bootstrap (run, bootstrap) where

import           System.Directory
import           System.Environment
import           System.Exit.Compat
import           System.FilePath
import           System.Posix.Files
import           System.Process

import           Main.Utils
import           PackageSets
import           Path

run :: IO ()
run = do
  binDir :: Path Bin <- getBinDir
  sandboxes <- getSandboxes
  [PackageSetName -> packageSetName] <- getArgs
  bootstrap binDir sandboxes packageSetName

bootstrap :: Path Bin  -> Path Sandboxes -> PackageSetName -> IO ()
bootstrap binDir sandboxesDir packageSetName = do
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
