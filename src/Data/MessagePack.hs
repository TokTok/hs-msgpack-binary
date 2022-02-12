{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy           #-}
{-# OPTIONS_GHC -Wno-orphans #-}
--------------------------------------------------------------------
-- |
-- Module    : Data.MessagePack
-- Copyright : (c) Hideyuki Tanaka, 2009-2015
-- License   : BSD3
--
-- Maintainer:  tanaka.hideyuki@gmail.com
-- Stability :  experimental
-- Portability: portable
--
-- Simple interface to pack and unpack MessagePack data.
--
--------------------------------------------------------------------

module Data.MessagePack (
  -- * Simple interface to pack and unpack msgpack binary
    pack
  , unpack
  , unpackEither
  , unpackValidate

  -- * Re-export modules
  -- $reexports
  , module X
  ) where

import           Control.Applicative    (Applicative)
import           Control.Monad          ((>=>))
import           Control.Monad.Validate (MonadValidate (..), runValidate)
import           Data.Binary            (Binary (..), decodeOrFail, encode)
import           Data.Binary.Get        (Get)
import qualified Data.ByteString.Lazy   as L

import           Data.MessagePack.Get   as X
import           Data.MessagePack.Put   as X
import           Data.MessagePack.Types as X


-- | Pack a Haskell value to MessagePack binary.
pack :: MessagePack a => a -> L.ByteString
pack = encode . toObject defaultConfig

-- | Unpack MessagePack binary to a Haskell value.
--
-- On failure, returns a list of error messages.
unpackValidate :: (MonadValidate DecodeError m, MessagePack a)
               => L.ByteString -> m a
unpackValidate = eitherToM . decodeOrFail >=> fromObjectWith defaultConfig
  where
    eitherToM (Left  (_, _, msg)) = refute $ decodeError msg
    eitherToM (Right (_, _, res)) = return res


unpackEither :: (MessagePack a)
               => L.ByteString -> Either DecodeError a
unpackEither = runValidate . unpackValidate


-- | Unpack MessagePack binary to a Haskell value. If it fails, it fails in the
-- Monad. In the Maybe monad, failure returns Nothing.
#if (MIN_VERSION_base(4,13,0))
unpack :: (Applicative m, Monad m, MonadFail m, MessagePack a)
#else
unpack :: (Applicative m, Monad m, MessagePack a)
#endif
       => L.ByteString -> m a
unpack = eitherToM . unpackEither
  where
    eitherToM (Left msgs) = fail $ show msgs
    eitherToM (Right res) = return res


instance Binary Object where
  get = getObject
  {-# INLINE get #-}

  put = putObject
  {-# INLINE put #-}

instance MonadValidate DecodeError Get where
    refute = fail . show
    tolerate m = m >> return Nothing
