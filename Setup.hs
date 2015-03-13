
import           Control.Exception
import           Control.Monad
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Process

main = defaultMainWithHooks simpleUserHooks{
  postCopy = bootstrapSandbox
}

bootstrapSandbox _ _ _ localBuildInfo = do
  hPutStrLn stderr "bootstrapping staskell packages..."
  let dataDir =
        (fromPathTemplate $ datadir $ installDirTemplates localBuildInfo) </>
        (fromPathTemplate $ datasubdir $ installDirTemplates localBuildInfo)
      sandboxDir = dataDir </> "sandbox"
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
