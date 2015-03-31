{-# LANGUAGE EmptyDataDecls #-}

module Path where

import           Data.String
import           System.FilePath

data PackageSetName = PackageSetName {fromPackageSetName :: String}
  deriving (Eq, Ord, Show)

instance IsString PackageSetName where
  fromString = PackageSetName

data Path a = Path {toPath :: FilePath}
  deriving (Show, Eq)
data Sandboxes
data Sandbox
data CabalConfig
data CabalSandboxConfig
data Bin
data Link
data ProgName
data Data
data Script

getCabalConfig :: Path Sandbox -> Path CabalConfig
getCabalConfig (Path dir) = Path (dir </> "cabal.config")

getCabalSandboxConfig :: Path Sandbox -> Path CabalSandboxConfig
getCabalSandboxConfig (Path dir) = Path (dir </> "cabal.sandbox.config")

mkProgramLink :: Path Bin -> PackageSetName -> Path Link
mkProgramLink (Path binDir) (PackageSetName name) =
  Path (binDir </> "runstaskell-" ++ name)
