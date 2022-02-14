{-# LANGUAGE StrictData  #-}
{-# LANGUAGE Trustworthy #-}
module Data.MessagePackSpec where

import           Test.Hspec

import qualified Data.MessagePack            as Impl
import           Test.MessagePack.BytePacker (BytePacker)
import qualified Test.MessagePack.BytePacker as BytePacker
import qualified Test.MessagePack.Spec       as MessagePackSpec


data Packer = Packer

instance BytePacker Packer where
    pack Packer = Impl.pack
    unpackValidate Packer = Impl.unpackValidate


spec :: Spec
spec = MessagePackSpec.spec Packer
