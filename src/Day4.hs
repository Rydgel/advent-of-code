{-# LANGUAGE OverloadedStrings #-}

module Day4 where

import qualified Data.ByteString.Char8 as B
import qualified Crypto.Hash.MD5 as MD5
import qualified Text.Printf as P

-- stack ghci --ghci-options "-XOverloadedStrings"

{- Day 4: The Ideal Stocking Stuffer -}

type Secret = B.ByteString
type Hash = String

secret :: Secret
secret = "iwrupvqb"

hexify :: B.ByteString -> String
hexify h = B.unpack h >>= P.printf "%02x"

md5ify :: Secret -> Int -> Hash
md5ify s n = hexify $ MD5.hash $ s `B.append` B.pack (show n)

isGoodHash :: Int -> Hash -> Bool
isGoodHash n h = replicate n '0' == take n h

mineHash :: Int -> Int -> Int
mineHash m n =
  if isGoodHash m $ md5ify secret n
  then n
  else mineHash m (n+1)

day4 :: IO ()
day4 = print $ mineHash 5 0

{- Part Two -}

day4' :: IO ()
day4' = print $ mineHash 6 0
