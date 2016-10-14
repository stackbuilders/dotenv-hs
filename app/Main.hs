{-# LANGUAGE CPP #-}

module Main where

#if MIN_VERSION_optparse_applicative(0,13,0)
import Data.Monoid ((<>))
#endif

import Options.Applicative

import Configuration.Dotenv (loadFile)

import Control.Monad.IO.Class(MonadIO(..))

import System.Process (system)
import System.Exit (exitWith)

data Options = Options
  { files    :: [String]
  , program  :: String
  , overload :: Bool
  } deriving (Show)

main :: IO ()
main = execParser opts >>= dotEnv
  where
    opts = info (helper <*> config)
      ( fullDesc
     <> progDesc "Runs PROGRAM after loading options from FILE"
     <> header "dotenv - loads options from dotenv files" )

config :: Parser Options
config = Options
     <$> some (strOption (
                  long "file"
                  <> short 'f'
                  <> metavar "FILE"
                  <> help "File to read for options" ))

     <*> argument str (metavar "PROGRAM")

     <*> switch ( long "overload"
                  <> short 'o'
                  <> help "Specify this flag to override existing variables" )

dotEnv :: MonadIO m => Options -> m ()
dotEnv opts = liftIO $ do
  mapM_ (loadFile (overload opts)) (files opts)
  code <- system (program opts)
  exitWith code
