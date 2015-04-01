
module Run where

import           Data.Foldable      (forM_)
import           Data.List
import           Data.Map           (keys)
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO

import           Bootstrap
import           PackageSets
import           Path
import           Runstaskell
import           Sandboxes

run :: Path ProgName -> Path Bin -> Path Data -> IO ()
run progName binDir dataDir = do
  args <- getArgs
  case parseOptions progName args of
    Help message -> runHelp message
    Error message -> runError message
    ListInstallable -> runListInstallable
    ListBootstrapped -> runListBootstrapped dataDir
    Bootstrap name -> runBootstrap binDir (getSandboxes dataDir) name
    RunScript script args -> runScript progName (getSandboxes dataDir) script args

data Options
  = Help String
  | Error String
  | ListInstallable
  | ListBootstrapped
  | Bootstrap PackageSetName
  | RunScript {
    script :: Path Script,
    arguments :: [String]
  }
  deriving (Show, Eq)

parseOptions :: Path ProgName -> [String] -> Options
parseOptions progName args = case args of
  ["--help"] -> Help (helpText progName)
  ["--list-installable"] -> ListInstallable
  ["--list-bootstrapped"] -> ListBootstrapped
  ["--bootstrap", packageSetName] -> Bootstrap $ PackageSetName packageSetName
  (invalid : invalids) | "-" `isPrefixOf` invalid && "--help" `elem` invalids -> Help (helpText progName)
  (invalid : _) | "-" `isPrefixOf` invalid -> Error ("invalid option: " ++ invalid)
  (script : args) -> RunScript (Path script :: Path Script) args
  [] -> Help (helpText progName)

helpText :: Path ProgName -> String
helpText progName = intercalate "\n" $
  "usage:" :
  ("  " ++ toPath progName ++ " SCRIPT [ARG1 ARG2 ...]") :
   "    run the given script with the given arguments" :
  ("  " ++ toPath progName ++ " --list-installable") :
   "    list all installable package sets" :
  ("  " ++ toPath progName ++ " --list-bootstrapped") :
   "    list all bootstrapped (locally installed) package sets" :
  ("  " ++ toPath progName ++ " --bootstrap PACKAGE_SET") :
   "    bootstrap (install locally) the given package sets" :
  ("  " ++ toPath progName ++ " --help") :
   "    output this help message" :
  []

runHelp :: String -> IO ()
runHelp = putStrLn

runError :: String -> IO ()
runError message = do
  hPutStrLn stderr message
  exitWith $ ExitFailure 1

runListInstallable :: IO ()
runListInstallable = mapM_ putStrLn $ map fromPackageSetName $ keys packageSets

runListBootstrapped :: Path Data -> IO ()
runListBootstrapped dataDir = do
  bootstrapped <- getBootstrappedSandboxes (getSandboxes dataDir)
  forM_ bootstrapped $ \ sandbox ->
    putStrLn (takeFileName (toPath sandbox))
