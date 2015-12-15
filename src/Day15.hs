{-# LANGUAGE OverloadedStrings #-}

module Day15 where

import qualified Data.Text as T
import           Data.Attoparsec.Text
import           Data.Either

{- Day 15: Science for Hungry People -}

-- Types

type Teaspoons = Int
type Ingredient = [Int]
type Recipe = [(Int, Ingredient)]

-- Parsing

parseIngredient :: Parser Ingredient
parseIngredient = do
  _ <- many1 letter
  _ <- string ": capacity "
  c <- signed decimal
  _ <- string ", durability "
  d <- signed decimal
  _ <- string ", flavor "
  f <- signed decimal
  _ <- string ", texture "
  t <- signed decimal
  _ <- string ", calories "
  cs <- signed decimal
  return [c,d,f,t,cs]

-- Logic

partitions :: Int -> Int -> [[Int]]
partitions total n
  | n > 1 = [x : xs | x <- [0..total], xs <- partitions (total-x) (n-1)]
  | otherwise = [[total]]

score :: Recipe -> Int
score = product . map (max 0) . foldl1 (zipWith (+)) . map (init . uncurry (map . (*)))

recipes :: Int -> [Ingredient] -> [Recipe]
recipes total ingredients =
  [zip amounts ingredients | amounts <- partitions total (length ingredients)]

calories :: Recipe -> Int
calories = sum . map (\(amt,xs) -> amt * last xs)

day15 :: IO ()
day15 = do
  fileStr <- readFile "resources/day15.txt"
  let is = rights $ map (parseOnly parseIngredient) $ T.lines $ T.pack fileStr
  print . maximum . map score . recipes 100 $ is

{- Part Two -}

day15' :: IO ()
day15' = do
  fileStr <- readFile "resources/day15.txt"
  let is = rights $ map (parseOnly parseIngredient) $ T.lines $ T.pack fileStr
  print . maximum . map score . filter ((500==) . calories) . recipes 100 $ is
