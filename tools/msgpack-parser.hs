module Main where

import           Data.MessagePack        (pack, unpack)
import           Test.MessagePack.Parser (parse)


main :: IO ()
main = parse pack unpack
