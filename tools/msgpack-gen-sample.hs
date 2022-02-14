module Main where

import           Control.Monad                    (when)
import qualified Data.ByteString.Lazy             as L
import           Data.Int                         (Int64)
import           Data.MessagePack                 (Object (..), pack)
import           Data.MessagePack.Arbitrary       ()
import           Data.Time.Clock                  (diffUTCTime, getCurrentTime)
import           System.Environment               (getArgs)
import           System.IO                        (hPutStr, hPutStrLn, stderr)
import           Test.QuickCheck.Arbitrary        (arbitrary)
import qualified Test.QuickCheck.Gen              as Gen
import           Test.QuickCheck.Instances.Vector ()


showBytes :: Int64 -> String
showBytes size
  | size > 10 * (1024 * 1024) = show (size `div` (1024 * 1024)) <> " MiB"
  | size > 10 * 1024 = show (size `div` 1024) <> " KiB"
  | otherwise = show size <> " B"


showSpeed :: Int64 -> Double -> String
showSpeed size time =
    show (fromIntegral (size `div` (1024 * 1024)) / time) <> " MiB/s"


main :: IO ()
main = do
    size:_ <- (++[30]) . map read <$> getArgs

    start <- getCurrentTime
    hPutStrLn stderr "Generating sample..."

    sample@(ObjectArray array) <- ObjectArray <$> Gen.generate (Gen.resize size arbitrary)
    when (sample == sample) $  -- force deep evaluation of the whole structure (kind of deepseq)
        hPutStr stderr $ "Generated msgpack array of length " <> show (length array)
    sampleTime <- getCurrentTime
    hPutStrLn stderr $ " in " <> show (diffUTCTime sampleTime start)

    let packed = pack sample
    hPutStr stderr $ "Message packed into " <> showBytes (L.length packed)
    packTime <- getCurrentTime
    hPutStrLn stderr $ " in " <> show (diffUTCTime packTime sampleTime)

    hPutStrLn stderr $ "Packing speed: " <> showSpeed (L.length packed) (realToFrac (diffUTCTime packTime sampleTime))

    L.putStr packed
