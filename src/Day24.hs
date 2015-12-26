{-# LANGUAGE OverloadedStrings #-}

module Day24 where

import Data.List

{- Day 24: It Hangs in the Balance -}

parse :: String -> [Int]
parse = map read . lines

combinations :: [a] -> Int -> [[a]]
combinations  _ 0 = [[]]
combinations xs n = [ y:ys
                    | y:xs' <- tails xs
                    , ys    <- combinations xs' $ n-1
                    ]

run :: [Int] -> Int -> Int
run wts nGroups =
    minimum $ head [ quantumEntanglements
                   | cs <- map (combinations wts) [1..]
                   , let quantumEntanglements = [ product c | c <- cs, sum c == groupSize ]
                   , not $ null quantumEntanglements
                   ]
    where groupSize = sum wts `div` nGroups


day24 :: IO ()
day24 = do
  fileStr <- readFile "resources/day24.txt"
  print $ run (parse fileStr) 3

{- Part Two -}

day24' :: IO ()
day24' = do
  fileStr <- readFile "resources/day24.txt"
  print $ run (parse fileStr) 4
