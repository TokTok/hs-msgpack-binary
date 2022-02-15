module Main where

import           Data.MessagePack          (pack)
import           Test.MessagePack.Generate (generate)


main :: IO ()
main = generate pack
