{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE Trustworthy #-}

--------------------------------------------------------------------
-- |
-- Module    : Data.MessagePack.Get
-- Copyright : (c) Hideyuki Tanaka, 2009-2015
-- License   : BSD3
--
-- Maintainer:  tanaka.hideyuki@gmail.com
-- Stability :  experimental
-- Portability: portable
--
-- MessagePack Deserializer using @Data.Binary@
--
--------------------------------------------------------------------

module Data.MessagePack.Get
  ( getObject
  , getNil
  , getBool
  , getInt
  , getWord
  , getFloat
  , getDouble
  , getStr
  , getBin
  , getArray
  , getMap
  , getExt
  ) where

import           Control.Applicative    (empty, (<$), (<$>), (<*>), (<|>))
import           Control.Monad          (guard, replicateM)
import           Data.Binary            (Get)
import           Data.Binary.Get        (getByteString, getWord16be,
                                         getWord32be, getWord64be, getWord8)
import           Data.Binary.IEEE754    (getFloat32be, getFloat64be)
import           Data.Bits              ((.&.))
import qualified Data.ByteString        as S
import           Data.Int               (Int16, Int32, Int64, Int8)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Data.Vector            as V
import           Data.Word              (Word64, Word8)

import           Data.MessagePack.Tags
import           Data.MessagePack.Types (Object (..))

getObject :: Get Object
getObject =
      ObjectNil    <$  getNil
  <|> ObjectBool   <$> getBool
  <|> ObjectInt    <$> getInt
  <|> ObjectWord   <$> getWord
  <|> ObjectFloat  <$> getFloat
  <|> ObjectDouble <$> getDouble
  <|> ObjectStr    <$> getStr
  <|> ObjectBin    <$> getBin
  <|> ObjectArray  <$> getArray getObject
  <|> ObjectMap    <$> getMap getObject getObject
  <|> uncurry ObjectExt <$> getExt

getNil :: Get ()
getNil = tag TAG_nil

getBool :: Get Bool
getBool =
  False <$ tag TAG_false <|>
  True  <$ tag TAG_true

getInt :: Get Int64
getInt =
  getWord8 >>= \case
    c | c .&. 0xE0 == 0xE0 ->
        return $ fromIntegral (fromIntegral c :: Int8)
    TAG_int_8  -> fromIntegral <$> getInt8
    TAG_int_16 -> fromIntegral <$> getInt16be
    TAG_int_32 -> fromIntegral <$> getInt32be
    TAG_int_64 -> fromIntegral <$> getInt64be
    _    -> empty

getWord :: Get Word64
getWord =
  getWord8 >>= \case
    c | c .&. 0x80 == 0x00 ->
        return $ fromIntegral c
    TAG_uint_8  -> fromIntegral <$> getWord8
    TAG_uint_16 -> fromIntegral <$> getWord16be
    TAG_uint_32 -> fromIntegral <$> getWord32be
    TAG_uint_64 -> fromIntegral <$> getWord64be
    _    -> empty

getFloat :: Get Float
getFloat = tag TAG_float_32 >> getFloat32be

getDouble :: Get Double
getDouble = tag TAG_float_64 >> getFloat64be

getStr :: Get T.Text
getStr = do
  len <- getWord8 >>= \case
    t | t .&. 0xE0 == 0xA0 ->
      return $ fromIntegral $ t .&. 0x1F
    TAG_str_8  -> fromIntegral <$> getWord8
    TAG_str_16 -> fromIntegral <$> getWord16be
    TAG_str_32 -> fromIntegral <$> getWord32be
    _    -> empty
  bs <- getByteString len
  case T.decodeUtf8' bs of
    Left  _ -> empty
    Right v -> return v

getBin :: Get S.ByteString
getBin = do
  len <- getWord8 >>= \case
    TAG_bin_8  -> fromIntegral <$> getWord8
    TAG_bin_16 -> fromIntegral <$> getWord16be
    TAG_bin_32 -> fromIntegral <$> getWord32be
    _    -> empty
  getByteString len

getArray :: Get a -> Get (V.Vector a)
getArray g = do
  len <- getWord8 >>= \case
    t | t .&. 0xF0 == 0x90 ->
      return $ fromIntegral $ t .&. 0x0F
    TAG_array_16 -> fromIntegral <$> getWord16be
    TAG_array_32 -> fromIntegral <$> getWord32be
    _    -> empty
  V.replicateM len g

getMap :: Get a -> Get b -> Get (V.Vector (a, b))
getMap k v = do
  len <- getWord8 >>= \case
    t | t .&. 0xF0 == 0x80 ->
      return $ fromIntegral $ t .&. 0x0F
    TAG_map_16 -> fromIntegral <$> getWord16be
    TAG_map_32 -> fromIntegral <$> getWord32be
    _    -> empty
  V.replicateM len $ (,) <$> k <*> v

getExt :: Get (Word8, S.ByteString)
getExt = do
  len <- getWord8 >>= \case
    TAG_fixext_1  -> return 1
    TAG_fixext_2  -> return 2
    TAG_fixext_4  -> return 4
    TAG_fixext_8  -> return 8
    TAG_fixext_16 -> return 16
    TAG_ext_8     -> fromIntegral <$> getWord8
    TAG_ext_16    -> fromIntegral <$> getWord16be
    TAG_ext_32    -> fromIntegral <$> getWord32be
    _    -> empty
  (,) <$> getWord8 <*> getByteString len

getInt8 :: Get Int8
getInt8 = fromIntegral <$> getWord8

getInt16be :: Get Int16
getInt16be = fromIntegral <$> getWord16be

getInt32be :: Get Int32
getInt32be = fromIntegral <$> getWord32be

getInt64be :: Get Int64
getInt64be = fromIntegral <$> getWord64be

tag :: Word8 -> Get ()
tag t = do
  b <- getWord8
  guard $ t == b
