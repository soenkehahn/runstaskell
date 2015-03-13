
module MainSpec where

import           Data.Foldable
import           System.Environment
import           System.FilePath
import           System.IO.Silently
import           System.Process
import           Test.Hspec

spec :: Spec
spec = do
  describe "runstaskell" $ do
    forM_ ["01", "02"] $ \ number -> do
      let scriptFile = "test" </> number <.> "hs"
      it ("executes " ++ scriptFile) $ do
        path <- getEnv "PATH"
        setEnv "PATH" ("./.cabal-sandbox/bin:" ++ path)
        output <- capture_ $ callCommand scriptFile
        output `shouldContain` (number ++ "-success")
