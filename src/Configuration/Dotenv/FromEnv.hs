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
module Configuration.Dotenv.FromEnv (FromEnv (..), fromEnv_) where

import           Control.Applicative              (liftA2)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           GHC.Generics

import           Configuration.Dotenv.Environment (lookupEnv)

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

-- | Convert from a field name to an environment variable name.
type NameConverter = String -> Maybe String

-- | Class for things that can be created from environment variables.
class FromEnv a where
  -- | Try to construct a value from environment variables.
  fromEnv :: (MonadIO m) => NameConverter -> m (Maybe a)
  default fromEnv :: (MonadIO m, Generic a, FromEnv' (Rep a)) => NameConverter -> m (Maybe a)
  fromEnv converter = fmap to <$> fromEnv' converter

-- | Use 'id' as the name converter.
fromEnv_ :: (FromEnv a, MonadIO m) => m (Maybe a)
fromEnv_ = fromEnv Just

class FromEnv' f where
  fromEnv' :: (MonadIO m) => NameConverter -> m (Maybe (f a))

instance {-# OVERLAPPING #-} FromEnv' f => FromEnv' (M1 i c f) where
  fromEnv' converter = fmap M1 <$> fromEnv' converter

instance (FromEnv' f, FromEnv' g) => FromEnv' (f :*: g)  where
  fromEnv' converter = do
    f' <- fromEnv' @f converter
    g' <- fromEnv' @g converter
    return $ liftA2 (:*:) f' g'

instance {-# OVERLAPPING #-} (Selector s, TrivialParse a) => FromEnv' (M1 S s (K1 i a)) where
  fromEnv' converter = do
    let m :: M1 i s f a
        m = undefined
        name = converter $ selName m
    case name of
      Just name' -> do
        c <- liftIO $ lookupEnv name'
        return $ fmap (M1 . K1) (triviallyParse =<< c)
      Nothing -> return Nothing
