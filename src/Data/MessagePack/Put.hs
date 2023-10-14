{-# LANGUAGE LambdaCase #-}
--------------------------------------------------------------------
-- |
-- Module    : Data.MessagePack.Put
-- Copyright : (c) Hideyuki Tanaka, 2009-2015
-- License   : BSD3
--
-- Maintainer:  tanaka.hideyuki@gmail.com
-- Stability :  experimental
-- Portability: portable
--
-- MessagePack Serializer using @Data.Binary@
--
--------------------------------------------------------------------

module Data.MessagePack.Put
  ( putObject
  , putNil
  , putBool
  , putInt
  , putWord
  , putFloat
  , putDouble
  , putStr
  , putBin
  , putArray
  , putMap
  , putExt
  ) where

import           Data.Binary            (Put)
import           Data.Binary.IEEE754    (putFloat32be, putFloat64be)
import           Data.Binary.Put        (putByteString, putWord16be,
                                         putWord32be, putWord64be, putWord8)
import           Data.Bits              ((.|.))
import qualified Data.ByteString        as S
import           Data.Int               (Int64)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Data.Vector            as V
import           Data.Word              (Word64, Word8)

import           Prelude                hiding (putStr)

import           Data.MessagePack.Tags
import           Data.MessagePack.Types (Object (..))


putObject :: Object -> Put
putObject = \case
  ObjectNil      -> putNil
  ObjectBool   b -> putBool b
  ObjectInt    n -> putInt n
  ObjectWord   n -> putWord n
  ObjectFloat  f -> putFloat f
  ObjectDouble d -> putDouble d
  ObjectStr    t -> putStr t
  ObjectBin    b -> putBin b
  ObjectArray  a -> putArray putObject a
  ObjectMap    m -> putMap putObject putObject m
  ObjectExt  b r -> putExt b r

putNil :: Put
putNil = putWord8 TAG_nil

putBool :: Bool -> Put
putBool False = putWord8 TAG_false
putBool True  = putWord8 TAG_true

putInt :: Int64 -> Put
putInt n
  | -0x20 <= n && n < 0x80 =
                            putWord8    (fromIntegral n)
  | 0     <= n && n < 0x100 =
    putWord8 TAG_uint_8  >> putWord8    (fromIntegral n)
  | 0     <= n && n < 0x10000 =
    putWord8 TAG_uint_16 >> putWord16be (fromIntegral n)
  | 0     <= n && n < 0x100000000 =
    putWord8 TAG_uint_32 >> putWord32be (fromIntegral n)
  | 0     <= n =
    putWord8 TAG_uint_64 >> putWord64be (fromIntegral n)
  | -0x80 <= n =
    putWord8 TAG_int_8   >> putWord8    (fromIntegral n)
  | -0x8000 <= n =
    putWord8 TAG_int_16  >> putWord16be (fromIntegral n)
  | -0x80000000 <= n =
    putWord8 TAG_int_32  >> putWord32be (fromIntegral n)
  | otherwise =
    putWord8 TAG_int_64  >> putWord64be (fromIntegral n)

putWord :: Word64 -> Put
putWord n
  | n < 0x80 =
                            putWord8    (fromIntegral n)
  | n < 0x100 =
    putWord8 TAG_uint_8  >> putWord8    (fromIntegral n)
  | n < 0x10000 =
    putWord8 TAG_uint_16 >> putWord16be (fromIntegral n)
  | n < 0x100000000 =
    putWord8 TAG_uint_32 >> putWord32be (fromIntegral n)
  | otherwise =
    putWord8 TAG_uint_64 >> putWord64be n

putFloat :: Float -> Put
putFloat f = do
  putWord8 TAG_float_32
  putFloat32be f

putDouble :: Double -> Put
putDouble d = do
  putWord8 TAG_float_64
  putFloat64be d

putStr :: T.Text -> Put
putStr t = do
  let bs = T.encodeUtf8 t
  case S.length bs of
    len | len <= 31 ->
          putWord8 $ 0xA0 .|. fromIntegral len
        | len < 0x100 ->
          putWord8 TAG_str_8  >> putWord8    (fromIntegral len)
        | len < 0x10000 ->
          putWord8 TAG_str_16 >> putWord16be (fromIntegral len)
        | otherwise ->
          putWord8 TAG_str_32 >> putWord32be (fromIntegral len)
  putByteString bs

putBin :: S.ByteString -> Put
putBin bs = do
  case S.length bs of
    len | len < 0x100 ->
          putWord8 TAG_bin_8  >> putWord8    (fromIntegral len)
        | len < 0x10000 ->
          putWord8 TAG_bin_16 >> putWord16be (fromIntegral len)
        | otherwise ->
          putWord8 TAG_bin_32 >> putWord32be (fromIntegral len)
  putByteString bs

putArray :: (a -> Put) -> V.Vector a -> Put
putArray p xs = do
  case V.length xs of
    len | len <= 15 ->
          putWord8 $ 0x90 .|. fromIntegral len
        | len < 0x10000 ->
          putWord8 TAG_array_16 >> putWord16be (fromIntegral len)
        | otherwise ->
          putWord8 TAG_array_32 >> putWord32be (fromIntegral len)
  V.mapM_ p xs

putMap :: (a -> Put) -> (b -> Put) -> V.Vector (a, b) -> Put
putMap p q xs = do
  case V.length xs of
    len | len <= 15 ->
          putWord8 $ 0x80 .|. fromIntegral len
        | len < 0x10000 ->
          putWord8 TAG_map_16 >> putWord16be (fromIntegral len)
        | otherwise ->
          putWord8 TAG_map_32 >> putWord32be (fromIntegral len)
  V.mapM_ (\(a, b) -> p a >> q b) xs

putExt :: Word8 -> S.ByteString -> Put
putExt typ dat = do
  case S.length dat of
    1  -> putWord8 TAG_fixext_1
    2  -> putWord8 TAG_fixext_2
    4  -> putWord8 TAG_fixext_4
    8  -> putWord8 TAG_fixext_8
    16 -> putWord8 TAG_fixext_16
    len | len < 0x100   -> putWord8 TAG_ext_8  >> putWord8    (fromIntegral len)
        | len < 0x10000 -> putWord8 TAG_ext_16 >> putWord16be (fromIntegral len)
        | otherwise     -> putWord8 TAG_ext_32 >> putWord32be (fromIntegral len)
  putWord8 typ
  putByteString dat
