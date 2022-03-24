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

module Data.MessagePack.Get (getObject) where

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
getObject = getWord8 >>= \case
  TAG_nil       -> pure ObjectNil
  TAG_false     -> pure $ ObjectBool False
  TAG_true      -> pure $ ObjectBool True
  c | c .&. 0xE0 == 0xE0 -> pure $ ObjectInt $ fromIntegral (fromIntegral c :: Int8)
  TAG_int_8     -> ObjectInt . fromIntegral <$> getInt8
  TAG_int_16    -> ObjectInt . fromIntegral <$> getInt16be
  TAG_int_32    -> ObjectInt . fromIntegral <$> getInt32be
  TAG_int_64    -> ObjectInt . fromIntegral <$> getInt64be
  c | c .&. 0x80 == 0x00 -> pure $ ObjectWord $ fromIntegral c
  TAG_uint_8    -> ObjectWord . fromIntegral <$> getWord8
  TAG_uint_16   -> ObjectWord . fromIntegral <$> getWord16be
  TAG_uint_32   -> ObjectWord . fromIntegral <$> getWord32be
  TAG_uint_64   -> ObjectWord . fromIntegral <$> getWord64be
  TAG_float_32  -> ObjectFloat  <$> getFloat32be
  TAG_float_64  -> ObjectDouble <$> getFloat64be
  t | t .&. 0xE0 == 0xA0 -> let len = fromIntegral $ t .&. 0x1F in ObjectStr <$> (getByteString len >>= decodeStr)
  TAG_str_8     -> ObjectStr <$> (fromIntegral <$> getWord8    >>= getByteString >>= decodeStr)
  TAG_str_16    -> ObjectStr <$> (fromIntegral <$> getWord16be >>= getByteString >>= decodeStr)
  TAG_str_32    -> ObjectStr <$> (fromIntegral <$> getWord32be >>= getByteString >>= decodeStr)
  TAG_bin_8     -> ObjectBin <$> (fromIntegral <$> getWord8    >>= getByteString)
  TAG_bin_16    -> ObjectBin <$> (fromIntegral <$> getWord16be >>= getByteString)
  TAG_bin_32    -> ObjectBin <$> (fromIntegral <$> getWord32be >>= getByteString)
  t | t .&. 0xF0 == 0x90 -> let len = fromIntegral $ t .&. 0x0F in ObjectArray <$> V.replicateM len getObject
  TAG_array_16  -> fromIntegral <$> getWord16be >>= \len -> ObjectArray <$> V.replicateM len getObject
  TAG_array_32  -> fromIntegral <$> getWord32be >>= \len -> ObjectArray <$> V.replicateM len getObject
  t | t .&. 0xF0 == 0x80 -> let len =  fromIntegral $ t .&. 0x0F in ObjectMap <$> V.replicateM len ((,) <$> getObject <*> getObject)
  TAG_map_16    -> fromIntegral <$> getWord16be >>= \len -> ObjectMap <$> V.replicateM len ((,) <$> getObject <*> getObject)
  TAG_map_32    -> fromIntegral <$> getWord32be >>= \len -> ObjectMap <$> V.replicateM len ((,) <$> getObject <*> getObject)
  TAG_fixext_1  -> ObjectExt <$> getWord8 <*> getByteString 1
  TAG_fixext_2  -> ObjectExt <$> getWord8 <*> getByteString 2
  TAG_fixext_4  -> ObjectExt <$> getWord8 <*> getByteString 4
  TAG_fixext_8  -> ObjectExt <$> getWord8 <*> getByteString 8
  TAG_fixext_16 -> ObjectExt <$> getWord8 <*> getByteString 16
  TAG_ext_8     -> fromIntegral <$> getWord8    >>= \len -> ObjectExt <$> getWord8 <*> getByteString len
  TAG_ext_16    -> fromIntegral <$> getWord16be >>= \len -> ObjectExt <$> getWord8 <*> getByteString len
  TAG_ext_32    -> fromIntegral <$> getWord32be >>= \len -> ObjectExt <$> getWord8 <*> getByteString len
  _ -> fail "Data.MessagePack.Get.getObject: Encountered invalid byte"

  where
    decodeStr bs = case T.decodeUtf8' bs of
      Left  _ -> fail "Data.MessagePack.Get.getObject: cannot decode bytestring to text"
      Right v -> pure v

getInt8 :: Get Int8
getInt8 = fromIntegral <$> getWord8

getInt16be :: Get Int16
getInt16be = fromIntegral <$> getWord16be

getInt32be :: Get Int32
getInt32be = fromIntegral <$> getWord32be

getInt64be :: Get Int64
getInt64be = fromIntegral <$> getWord64be
