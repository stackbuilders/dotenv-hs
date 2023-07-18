{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Configuration.Dotenv.FromEnv
(
    FromEnv (..)
  , toUpperSnake
  , defaultEnvOpts
  , GFromEnv (..)
  , FromEnvOptions (..)
) where

import           Control.Applicative              (liftA2)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           GHC.Generics
import           Text.Casing                      (screamingSnake)

import           Configuration.Dotenv.Environment (lookupEnv)


class FromEnv a where
  fromEnv :: (MonadIO m) => m (Maybe a)
  default fromEnv :: (MonadIO m, Generic a, GFromEnv' (Rep a)) => m (Maybe a)
  fromEnv = gFromEnv defaultEnvOpts

class TrivialParse a where
  triviallyParse :: String -> Maybe a

instance TrivialParse Int where
  triviallyParse = Just . read

instance TrivialParse String where
   triviallyParse = Just

instance TrivialParse Char where
   triviallyParse [c] = Just c
   triviallyParse _   = Nothing

instance TrivialParse Text where
  triviallyParse = Just . T.pack

type FieldLabelModifier = String -> Maybe String

newtype FromEnvOptions = FromEnvOptions
  { optsFieldLabelModifier :: FieldLabelModifier
  }

toUpperSnake :: FieldLabelModifier
toUpperSnake = Just . screamingSnake

defaultEnvOpts :: FromEnvOptions
defaultEnvOpts = FromEnvOptions
  { optsFieldLabelModifier = Just
  }

-- | Class for things that can be created from environment variables.
class GFromEnv a where
  -- | Try to construct a value from environment variables.
  gFromEnv :: (MonadIO m) => FromEnvOptions -> m (Maybe a)
  default gFromEnv :: (MonadIO m, Generic a, GFromEnv' (Rep a)) => FromEnvOptions -> m (Maybe a)
  gFromEnv opts = fmap to <$> gFromEnv' opts

instance (Generic a, GFromEnv' (Rep a)) => GFromEnv a

class GFromEnv' f where
  gFromEnv' :: (MonadIO m) => FromEnvOptions -> m (Maybe (f a))

instance {-# OVERLAPPING #-} GFromEnv' f => GFromEnv' (M1 i c f) where
  gFromEnv' converter = fmap M1 <$> gFromEnv' converter

instance (GFromEnv' f, GFromEnv' g) => GFromEnv' (f :*: g)  where
  gFromEnv' opts = do
    f' <- gFromEnv' @f opts
    g' <- gFromEnv' @g opts
    return $ liftA2 (:*:) f' g'

instance {-# OVERLAPPING #-} (Selector s, TrivialParse a) => GFromEnv' (M1 S s (K1 i a)) where
  gFromEnv' opts = do
    let m :: M1 i s f a
        m = undefined
        name = optsFieldLabelModifier opts $ selName m
    case name of
      Just name' -> do
        c <- liftIO $ lookupEnv name'
        return $ fmap (M1 . K1) (triviallyParse =<< c)
      Nothing -> return Nothing
