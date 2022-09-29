{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.Version         (showVersion)
#if !MIN_VERSION_base(4,13,0)
import           Data.Monoid          ((<>))
#endif

import           Options.Applicative
import           Paths_dotenv         (version)

import           Control.Monad        (void)

import           Configuration.Dotenv (Config(..), defaultConfig, loadFile)

import           System.Exit          (exitWith)
import           System.Process       (system)

data Options = Options
  { dotenvFiles        :: [String]
  , dotenvExampleFiles :: [String]
  , override           :: Bool
  , verbose            :: Bool
  , program            :: String
  , args               :: [String]
  } deriving (Show)

main :: IO ()
main = do
  Options{..} <- execParser opts
  let configDotenv =
        Config
          { configExamplePath = dotenvExampleFiles
          , configOverride = override
          , configVerbose = verbose
          , configPath =
              if null dotenvFiles
                then configPath defaultConfig
                else dotenvFiles
          }
   in do
     void $ loadFile configDotenv
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

     <*> switch (  long "verbose"
                  <> help "Specify this flag to print out the variables loaded and other useful insights" )

     <*> argument str (metavar "PROGRAM")

     <*> many (argument str (metavar "ARG"))
