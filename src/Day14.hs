{-# LANGUAGE OverloadedStrings #-}

module Day14 where

import qualified Data.Text as T
import           Data.Attoparsec.Text
import           Data.Either
import           Data.List as L
import           Data.Ord

{- Day 14: Reindeer Olympics -}

type Name = String
data Stats = Stats Int Int Int
data Reindeer = Reindeer { name :: Name, stats :: Stats }

parseReindeer :: Parser Reindeer
parseReindeer = do
  n <- many1 letter
  _ <- string " can fly "
  d <- decimal
  _ <- string " km/s for "
  t <- decimal
  _ <- string " seconds, but then must rest for "
  r <- decimal
  _ <- string " seconds."
  return $ Reindeer n (Stats d t r)

fly :: Stats -> [Int]
fly (Stats d t r) = L.cycle (L.replicate t d ++ L.replicate r 0)

flight :: Int -> Stats -> Int
flight t s = sum $ L.take t $ fly s

day14 :: IO ()
day14 = do
  fileStr <- readFile "resources/day14.txt"
  let result = rights . map (parseOnly parseReindeer) $ T.lines $ T.pack fileStr
      ns = map name result
      ds = map (flight 2503 . stats) result
  mapM_ print $ sortBy (comparing snd) (zip ns ds)

{- Part Two -}

fly' :: Stats -> [Int]
fly' (Stats d t r) = tail $ scanl (+) 0 (cycle (replicate t d ++ replicate r 0))

bonuses' :: [Int] -> [Int]
bonuses' xs = map (\x -> if x == m then 1 else 0) xs
  where m = maximum xs

race :: [Stats] -> [[Int]]
race ss = scanl1 (zipWith (+)) $ map bonuses' $ transpose (map fly' ss)

day14' :: IO ()
day14' = do
  fileStr <- readFile "resources/day14.txt"
  let result = rights . map (parseOnly parseReindeer) $ T.lines $ T.pack fileStr
      ns = map name result
      ss = map stats result
  print $ zip ns $ race ss !! 2503
