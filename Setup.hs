
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
  callCommand "tree"
  callCommand "cabal sandbox init"
  callCommand "cabal install tagged -j1"
