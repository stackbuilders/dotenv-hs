-- |
-- Module      :  Configuration.Dotenv.Types
-- Copyright   :  © 2015–2020 Stack Builders Inc.
-- License     :  MIT
--
-- Maintainer  :  Stack Builders <hackage@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides a Data.Text interface for retrieving variables in a dotenv file.

module Configuration.Dotenv.Text (parseFile) where

import qualified Configuration.Dotenv

import           Control.Arrow          ((***))
import qualified Data.Text              as T

import           Control.Monad          (liftM)

import           Control.Monad.IO.Class (MonadIO (..))

-- | Parses the given dotenv file and returns values /without/ adding them to
-- the environment.
parseFile ::
  MonadIO m =>
  FilePath -- ^ A file containing options to read
  -> m [(T.Text, T.Text)] -- ^ Variables contained in the file
parseFile f = map (T.pack *** T.pack) `liftM` Configuration.Dotenv.parseFile f
