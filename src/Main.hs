
module Main where

import           System.Environment
import           System.Exit
import           System.Process

main :: IO ()
main = do
  getArgs >>= spawnProcess "runhaskell" >>= waitForProcess >>= exitWith
