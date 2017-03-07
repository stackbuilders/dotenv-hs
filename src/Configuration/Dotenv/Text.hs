{- |
-- Module      :  Configuration.Dotenv.Text
-- Copyright   :  © 2015–2017 Stack Builders Inc.
-- License     :  MIT
--
-- Maintainer  :  Stack Builders <hackage@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides a "Data.Text" interface for retrieving variables from a dotenv file.
-}

module Configuration.Dotenv.Text (parseFile) where

import Control.Arrow
import Control.Monad (liftM)
import Control.Monad.IO.Class
import qualified Data.Text as T

import qualified Configuration.Dotenv.File as F

-- | @parseFile@ parses the given dotenv file and returns values /without/ adding them to
-- the environment.
parseFile ::
  (Functor m, MonadIO m)
  => FilePath             -- ^ A file containing options to read
  -> m [(T.Text, T.Text)] -- ^ Variables contained in the file
parseFile path = map (T.pack *** T.pack) `liftM` F.parseFile path
