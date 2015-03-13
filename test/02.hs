#!/usr/bin/env runstaskell

import System.IO.Temp
import System.FilePath

main :: IO ()
main = withSystemTempDirectory "runstaskell-test" $ \ dir -> do
  writeFile (dir </> "foo") "02-success"
  putStrLn =<< readFile (dir </> "foo")
