{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.MessagePackBench (suite) where

import           Control.DeepSeq           (NFData)
import           Criterion.Main            (Benchmark, bench, bgroup, nf)
import qualified Data.ByteString.Lazy      as LBS
import           Data.Int                  (Int64)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import           Test.QuickCheck.Gen       (resize, unGen)
import           Test.QuickCheck.Random    (mkQCGen)

import           GHC.Generics

import           Data.MessagePack


defaultSeed :: Int
defaultSeed = 301

instance MessagePack a => MessagePack (Maybe a)

data Expr
  = I Int
  | Add Expr Expr
  | Prod Expr Expr
  deriving (NFData, Generic)

instance MessagePack Expr

expr :: Int -> Expr
expr i = foldr build (I 10) [0..i] where
  build x e =
    case x `mod` 4 of
      0 -> Add e e
      1 -> Prod e e
      2 -> Add e (I x)
      3 -> Prod (I x) e

arb :: Arbitrary a => Int -> a
arb size =
  let g = unGen $ resize size arbitrary in
  g (mkQCGen defaultSeed) defaultSeed


benchRange
  :: NFData b
  => Int -> Int -> Int -> (a -> b) -> (Int -> a) -> [Benchmark]
benchRange from to steps f g =
  map (\step ->
      let sz = from + ((to - from) `div` (steps - 1)) * step in
      bench (show sz) $ nf f (g sz)
    ) [0..steps-1]


suite :: [Benchmark]
suite =
  [ bgroup "pack"
    [ bgroup "Expr" $ benchRange 10 40 4 pack expr
    , bench "Just Int" $ nf pack (Just (3 :: Int))
    , bench "Nothing"  $ nf pack (Nothing :: Maybe Int)
    , bench "()"       $ nf pack ()
    , bgroup "[a]" $ benchRange 1000 10000 10 pack (`replicate` ())
      -- ^ should be linear
    ]
  , bgroup "unpack"
    [ bgroup "Expr" $ benchRange 10 100 10 (unpack :: LBS.ByteString -> Maybe Expr) (pack . expr)
    , bench "Just Int" $ nf (unpack :: LBS.ByteString -> Maybe Int) (pack (Just (3 :: Int)))
    , bench "Nothing"  $ nf (unpack :: LBS.ByteString -> Maybe Int) (pack (Nothing :: Maybe Int))
    , bench "()"       $ nf (unpack :: LBS.ByteString -> Maybe () ) (pack ())
    , bgroup "[a]" $ benchRange 1000 10000 10 pack (`replicate` ())
      -- ^ should be linear
    ]
  ]
