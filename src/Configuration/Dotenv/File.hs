{- |
-- Module      :  Configuration.Dotenv.File
-- Copyright   :  © 2015–2017 Stack Builders Inc.
-- License     :  MIT
--
-- Maintainer  :  Stack Builders <hackage@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides parsers for dotenv files.
-}

module Configuration.Dotenv.File where

import Control.Monad.IO.Class
import Text.Megaparsec
import Configuration.Dotenv.Parse (configParser)
import Configuration.Dotenv.ParsedVariable (interpolateParsedVariables)

-- | @parseFile@ parses a .env.example file.
-- If the file does not exist then it fails.
parseFile :: MonadIO m => FilePath -> m [(String, String)]
parseFile path = liftIO $ do
  content <- readFile path
  case parse configParser path content of
    Left err   -> error $ parseErrorPretty err
    Right vars -> liftIO $ interpolateParsedVariables vars
