{- |
-- Module      :  Configuration.Dotenv.Types
-- Copyright   :  © 2015–2017 Stack Builders Inc.
-- License     :  MIT
--
-- Maintainer  :  Stack Builders <hackage@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides the types with extra options for loading a dotenv file.
-}

module Configuration.Dotenv.Types where

-- | Configuration Data Types with extra options for executing dotenv.
data Config = Config
  { configExamplePath :: [FilePath] -- ^ The paths for the .env.example files
  , configOverride    :: Bool     -- ^ Flag to allow override env variables
  , configPath        :: [FilePath] -- ^ The paths for the .env files
  } deriving (Eq, Show)

defaultConfig :: Config
defaultConfig =
  Config
    { configExamplePath = []
    , configOverride = False
    , configPath = [ ".env" ]
    }
