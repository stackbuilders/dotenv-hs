{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

#if MIN_VERSION_optparse_applicative(0,13,0)
import Data.Monoid ((<>))
#endif

import Options.Applicative

import Control.Monad (void)

import System.Process (system)
import System.Exit (exitWith)

import Configuration.Dotenv
import Configuration.Dotenv.Types

data Options = Options
  { program           :: String -- ^ Program to run with the load env variables
  , args              :: [String] -- ^ Args for the program that is going to be executed
  , dotenvFile        :: [String] -- ^ Paths for the .env files
  , dotenvExampleFile :: [String] -- ^ Paths for the .env.example files
  , override          :: Bool   -- ^ Override current environment variables
  , safe              :: Bool   -- ^ Allow dotenv-safe
  } deriving (Show)

main :: IO ()
main = do
  Options{..} <- execParser opts
  void $ loadFile Config
    { configExamplePath = dotenvExampleFile
    , configOverride = override
    , configPath = dotenvFile
    , configSafe = safe
    }
  system (program ++ concatMap (" " ++) args) >>= exitWith
    where
      opts = info (helper <*> config)
        ( fullDesc
       <> progDesc "Runs PROGRAM after loading options from DOTENV FILE"
       <> header "dotenv - loads options from dotenv files" )

config :: Parser Options
config =
  Options
     <$> argument str (metavar "PROGRAM")

     <*> many (argument str (metavar "ARG"))

     <*> many
          (strOption
             ( long "dotenv"
            <> short 'e'
            <> showDefault
            <> metavar "DOTENV"
            <> help "Files with the env variables" ))

     <*> many
          (strOption
              ( long "dotenv-example"
             <> short 'x'
             <> showDefault
             <> metavar "DOTENV_EXAMPLE"
             <> help "Files with all the necesary env variables" ))

     <*> switch
          ( long "override"
         <> short 'o'
         <> help "Override existing variables" )

     <*> switch
          ( long "safe"
         <> short 's'
         <> help "Allow dotenv-safe" )
