module Day17 where

import Data.List
import Data.Ord

{- Day 17: No Such Thing as Too Much -}

parseContainers :: String -> [Int]
parseContainers = map read . lines

combinations :: [Int] -> [[Int]]
combinations = filter ((==150) . sum) . subsequences

day17 :: IO ()
day17 =
  readFile "resources/day17.txt"
    >>= print . length . combinations . parseContainers

{- Part Two -}

minContainers :: [Int] -> Int
minContainers xs =
  let ss = combinations xs
      mn = length $ minimumBy (comparing length) ss
  in  length $ filter ((==mn) . length) ss

day17' :: IO ()
day17' =
  readFile "resources/day17.txt"
    >>= print . minContainers . parseContainers
