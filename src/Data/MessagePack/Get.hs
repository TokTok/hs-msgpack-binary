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
-- MessagePack Deserializer using @Data.Persist@
--
--------------------------------------------------------------------

module Data.MessagePack.Get
  ( getObject
  ) where

import           Control.Applicative ((<$), (<$>), (<*>), (<|>))
import           Control.Monad (guard, replicateM)
import           Data.Bits ((.&.))
import qualified Data.ByteString as S
import           Data.Int (Int16, Int32, Int64, Int8)
import           Data.Persist (Get, get, unBE)
import qualified Data.Persist as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Word (Word64, Word32, Word16, Word8)

import           Data.MessagePack.Types (Object (..))

getObject :: Get Object
getObject = getWord8 >>= \case
  0xC0 -> pure ObjectNil
  0xC2 -> pure $ ObjectBool False
  0xC3 -> pure $ ObjectBool True
  c | c .&. 0xE0 == 0xE0 -> pure $ ObjectInt $ fromIntegral (fromIntegral c :: Int8)
  0xD0 -> ObjectInt . fromIntegral <$> getInt8
  0xD1 -> ObjectInt . fromIntegral <$> getInt16be
  0xD2 -> ObjectInt . fromIntegral <$> getInt32be
  0xD3 -> ObjectInt . fromIntegral <$> getInt64be
  c | c .&. 0x80 == 0x00 -> pure $ ObjectWord $ fromIntegral c
  0xCC -> ObjectWord . fromIntegral <$> getWord8
  0xCD -> ObjectWord . fromIntegral <$> getWord16be
  0xCE -> ObjectWord . fromIntegral <$> getWord32be
  0xCF -> ObjectWord . fromIntegral <$> getWord64be
  0xCA -> ObjectFloat <$> getFloat32be
  0xCB -> ObjectDouble <$> getFloat64be
  t | t .&. 0xE0 == 0xA0 -> let len = fromIntegral $ t .&. 0x1F in ObjectStr <$> (getByteString len >>= decodeStr)
  0xD9 -> ObjectStr <$> (fromIntegral <$> getWord8 >>= getByteString >>= decodeStr)
  0xDA -> ObjectStr <$> (fromIntegral <$> getWord16be >>= getByteString >>= decodeStr)
  0xDB -> ObjectStr <$> (fromIntegral <$> getWord32be >>= getByteString >>= decodeStr)
  0xC4 -> ObjectBin <$> (fromIntegral <$> getWord8 >>= getByteString)
  0xC5 -> ObjectBin <$> (fromIntegral <$> getWord16be >>= getByteString)
  0xC6 -> ObjectBin <$> (fromIntegral <$> getWord32be >>= getByteString)
  t | t .&. 0xF0 == 0x90 -> let len = fromIntegral $ t .&. 0x0F in ObjectArray <$> replicateM len getObject
  0xDC -> fromIntegral <$> getWord16be >>= \len -> ObjectArray <$> replicateM len getObject
  0xDD -> fromIntegral <$> getWord32be >>= \len -> ObjectArray <$> replicateM len getObject
  t | t .&. 0xF0 == 0x80 -> let len =  fromIntegral $ t .&. 0x0F in ObjectMap <$> replicateM len ((,) <$> getObject <*> getObject)
  0xDE -> fromIntegral <$> getWord16be >>= \len -> ObjectMap <$> replicateM len ((,) <$> getObject <*> getObject)
  0xDF -> fromIntegral <$> getWord32be >>= \len -> ObjectMap <$> replicateM len ((,) <$> getObject <*> getObject)
  0xD4 -> ObjectExt <$> getWord8 <*> getByteString 1
  0xD5 -> ObjectExt <$> getWord8 <*> getByteString 2
  0xD6 -> ObjectExt <$> getWord8 <*> getByteString 4
  0xD7 -> ObjectExt <$> getWord8 <*> getByteString 8
  0xD8 -> ObjectExt <$> getWord8 <*> getByteString 16
  0xC7 -> fromIntegral <$> getWord8 >>= \len -> ObjectExt <$> getWord8 <*> getByteString len
  0xC8 -> fromIntegral <$> getWord16be >>= \len -> ObjectExt <$> getWord8 <*> getByteString len
  0xC9 -> fromIntegral <$> getWord32be >>= \len -> ObjectExt <$> getWord8 <*> getByteString len
  _ -> fail "Data.MessagePack.Get.getObject: Encountered invalid byte"

  where
    decodeStr bs = case T.decodeUtf8' bs of
      Left  _ -> fail "Data.MessagePack.Get.getObject: cannot decode bytestring to text"
      Right v -> pure v

getWord8 :: Get Word8
getWord8 = get

getWord16be :: Get Word16
getWord16be = unBE <$> get

getWord32be :: Get Word32
getWord32be = unBE <$> get

getWord64be :: Get Word64
getWord64be = unBE <$> get

getFloat32be :: Get Float
getFloat32be = unBE <$> get

getFloat64be :: Get Double
getFloat64be = unBE <$> get

getByteString :: Int -> Get S.ByteString
getByteString = P.getByteString

getInt8 :: Get Int8
getInt8 = fromIntegral <$> getWord8

getInt16be :: Get Int16
getInt16be = fromIntegral <$> getWord16be

getInt32be :: Get Int32
getInt32be = fromIntegral <$> getWord32be

getInt64be :: Get Int64
getInt64be = fromIntegral <$> getWord64be
