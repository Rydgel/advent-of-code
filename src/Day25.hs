{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}

module Day25 where

import Text.Regex.PCRE.Heavy

parseCoord :: String -> (Integer,Integer)
parseCoord input = (r, c)
  where regex = [re|\d+|]
        [r, c] = map (read . fst) $ scan regex input

makeCode :: Integer -> Integer
makeCode = mc 20151125
  where mc !c 0 = c
        mc !c !n = mc (c * 252533 `rem` 33554393) $ n - 1

day25 :: IO ()
day25 = do
  fileStr <- readFile "resources/day25.txt"
  let (r,c) = parseCoord fileStr
      index = sum [1..r+c-2] + c - 1
  print $ makeCode index
