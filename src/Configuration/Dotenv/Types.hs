-- |
-- Module      :  Configuration.Dotenv.Types
-- Copyright   :  © 2015–2020 Stack Builders Inc.
-- License     :  MIT
--
-- Maintainer  :  Stack Builders <hackage@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides the types with extra options for loading a dotenv file.

module Configuration.Dotenv.Types
  ( Config(..)
  , defaultConfig
  )
  where

-- | Configuration Data Types with extra options for executing dotenv.
data Config = Config
  { configPath        :: [FilePath] -- ^ The paths for the .env files
  , configExamplePath :: [FilePath] -- ^ The paths for the .env.example files
  , configOverride    :: Bool     -- ^ Flag to allow override env variables
  } deriving (Eq, Show)

-- | Default configuration. Use .env file without .env.example strict envs and
-- without overriding.
defaultConfig :: Config
defaultConfig =
  Config
    { configExamplePath = []
    , configOverride = False
    , configPath = [ ".env" ]
    }
