
import           Control.Monad
import           Distribution.PackageDescription
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.Setup
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Process

main :: IO ()
main = defaultMainWithHooks simpleUserHooks{
  postCopy = bootstrapSandbox
}

bootstrapSandbox :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
bootstrapSandbox _ copyFlags pd localBuildInfo = do
  let paths :: InstallDirs FilePath
      paths = absoluteInstallDirs pd localBuildInfo (fromFlag $ copyDest copyFlags)
      dataDir = datadir paths
      sandboxDir = dataDir </> "sandbox"
  hPutStrLn stderr ("bootstrapping staskell packages into " ++ sandboxDir ++ "...")
  exists <- doesDirectoryExist sandboxDir
  when exists $
    removeDirectoryRecursive sandboxDir
  createDirectoryIfMissing True sandboxDir
  setCurrentDirectory sandboxDir
  callCommand "wget http://www.stackage.org/snapshot/lts-1.11/cabal.config?download=true -O cabal.config"
  callCommand "cabal sandbox init"
  callCommand ("cabal install " ++ unwords packages)

packages :: [String]
packages =
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
  "shell-conduit" :
  "shelly" :
  "silently" :
  "string-conversions" :
  "tagged" :
  "temporary" :
  "transformers" :
  "yaml" :
  []
