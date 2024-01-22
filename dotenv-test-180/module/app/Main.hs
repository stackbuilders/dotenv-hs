module Main where

import Configuration.Dotenv
import System.Environment (getEnv)

main :: IO ()
main = do
  loadFile defaultConfig

  putStrLn "Running from submodule"

  getEnv "FOO" >>= putStrLn
  getEnv "BAR" >>= putStrLn
