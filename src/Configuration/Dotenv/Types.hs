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
  { configExamplePath :: FilePath -- ^ The path for the .env.example file
  , configOverride    :: Bool     -- ^ Flag to allow override env variables
  , configPath        :: FilePath -- ^ The path for the .env file
  } deriving (Eq, Show)
