module Configuration.Dotenv.Text (parseFile) where

import qualified Configuration.Dotenv

import qualified Data.Text as T
import Control.Arrow ((***))

-- | Parses the given dotenv file and returns values /without/ adding them to
-- the environment.
parseFile ::
  FilePath -- ^ A file containing options to read
  -> IO [(T.Text, T.Text)] -- ^ Variables contained in the file
parseFile f = map (T.pack *** T.pack) `fmap` Configuration.Dotenv.parseFile f
