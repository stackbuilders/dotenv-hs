{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Version (showVersion)
#if MIN_VERSION_optparse_applicative(0,13,0)
import Data.Monoid ((<>))
#endif

import Options.Applicative
import Paths_dotenv (version)

import Control.Monad (when)

import Configuration.Dotenv (loadFile)
import Configuration.Dotenv.Types (Config(..), defaultConfig)
import Configuration.Dotenv.Scheme.Parser (readScheme, checkEnvTypes)

import System.Process (system)
import System.Exit (exitWith)

data Options = Options
  { dotenvFiles        :: [String]
  , dotenvExampleFiles :: [String]
  , override           :: Bool
  , program            :: String
  , args               :: [String]
  , safeModeEnabled    :: Bool
  } deriving (Show)

main :: IO ()
main = do
  Options{..} <- execParser opts
  envs <- loadFile Config
    { configExamplePath = dotenvExampleFiles
    , configOverride = override
    , configPath =
        if null dotenvFiles
          then configPath defaultConfig
          else dotenvFiles
    }
  when safeModeEnabled (readScheme >>= checkEnvTypes envs)
  system (program ++ concatMap (" " ++) args) >>= exitWith
    where
      opts = info (helper <*> versionOption <*> config)
        ( fullDesc
       <> progDesc "Runs PROGRAM after loading options from FILE"
       <> header "dotenv - loads options from dotenv files" )
      versionOption =
        infoOption
          (showVersion version)
          (long "version" <> short 'v' <> help "Show version of the program")

config :: Parser Options
config = Options
     <$> many (strOption (
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

     <*> switch ( long "safe"
                  <> short 's'
                  <> help "Reads the .scheme.yml file from the current directory to enable type checking of envs" )
