{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

#if MIN_VERSION_optparse_applicative(0,13,0)
import Data.Monoid ((<>))
#endif

import Options.Applicative

import Control.Monad (void)

import Configuration.Dotenv (loadFile)
import Configuration.Dotenv.Types (Config(..))

import System.Process (system)
import System.Exit (exitWith)

data Options = Options
  { dotenvFiles        :: [String]
  , dotenvExampleFiles :: [String]
  , override           :: Bool
  , program            :: String
  , args               :: [String]
  } deriving (Show)

main :: IO ()
main = do
  Options{..} <- execParser opts
  void $ loadFile Config
    { configExamplePath = dotenvExampleFiles
    , configOverride = override
    , configPath = dotenvFiles
    }
  system (program ++ concatMap (" " ++) args) >>= exitWith
    where
      opts = info (helper <*> config)
        ( fullDesc
       <> progDesc "Runs PROGRAM after loading options from FILE"
       <> header "dotenv - loads options from dotenv files" )

config :: Parser Options
config = Options
     <$> some (strOption (
                  long "dotenv"
                  <> short 'f'
                  <> metavar "DOTENV"
                  <> help "File to read for environmental variables" ))

     <*> many (strOption (
                  long "example"
                  <> metavar "DOTENV_EXAMPLE"
                  <> help "File to read for needed environmental variables" ))

     <*> switch ( long "overload"
                  <> short 'o'
                  <> help "Specify this flag to override existing variables" )

     <*> argument str (metavar "PROGRAM")

     <*> many (argument str (metavar "ARG"))

