{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Version (showVersion)
#if MIN_VERSION_optparse_applicative(0,13,0)
import Data.Monoid ((<>))
#endif

import Options.Applicative
import Paths_dotenv (version)

import Control.Monad (void, unless)

import Configuration.Dotenv
  (Config(..)
  , loadFile
  , loadSafeFile
  , defaultConfig
  , defaultValidatorMap)

import System.Process (system)
import System.Exit (exitWith)

data Options = Options
  { dotenvFiles        :: [String]
  , dotenvExampleFiles :: [String]
  , override           :: Bool
  , program            :: String
  , args               :: [String]
  , schemaFile         :: FilePath
  , disableSchema      :: Bool
  } deriving (Show)

main :: IO ()
main = do
  Options{..} <- execParser opts
  let configDotenv =
        Config
          { configExamplePath = dotenvExampleFiles
          , configOverride = override
          , configPath =
              if null dotenvFiles
                then configPath defaultConfig
                else dotenvFiles
          }
   in do
     void $ loadFile configDotenv
     unless disableSchema
       (void $ loadSafeFile defaultValidatorMap schemaFile configDotenv)
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

     <*> strOption ( long "schema"
                      <> short 's'
                      <> help "Set the file path for the schema.yml file"
                      <> value ".schema.yml" )

     <*> switch ( long "no-schema" <> help "Omit type checking" )
