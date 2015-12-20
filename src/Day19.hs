{-# LANGUAGE QuasiQuotes #-}

module Day19 where

import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.List (intercalate)
import Data.Maybe
import Data.String.Utils
import Text.Regex.PCRE.Heavy (re, scan)

{- Day 19: Medicine for Rudolph-}

parseMapping :: String -> (String, String)
parseMapping s = let [k, v] = snd . head $ scan [re|(\w+) => (\w+)|] s
                 in (k, v)

singleReplacements :: String -> String -> String -> [String]
singleReplacements k v src =
  let pieces = split k src
      f p (i',s) | i' == 0 = (i'-1, (p ++ v ++ head s) : tail s)
      f p (i',s)           = (i'-1, p : s)
      parts = [ snd $ foldr f (i, []) pieces
              | i <- [ 1 .. length pieces - 1 ]
              ]
  in map (intercalate k) parts


uniqueSubs :: [(String, String)] -> String -> HashSet String
uniqueSubs reps src = S.fromList $ concat [ singleReplacements k v src | (k, v) <- reps]

uniquePredecessors :: [(String, String)] -> String -> HashSet String
uniquePredecessors reps src = S.fromList $ concat [ singleReplacements v k src | (k, v) <- reps]

findPathToElectron :: [(String, String)] -> String -> Int
findPathToElectron reps = fromJust . go 0
    where go _ [] = Nothing
          go c "e" = Just c
          go c s = listToMaybe . mapMaybe (go (c+1))
                   . S.toList $ uniquePredecessors reps s

part1 :: String -> Int
part1 input = let (s:_:mappings) = reverse $ lines input
                  reps = map parseMapping mappings
              in S.size $ uniqueSubs reps s

day19 :: IO ()
day19 = do
  fileStr <- readFile "resources/day19.txt"
  print $ part1 fileStr

{- Part Two -}

part2 :: String -> Int
part2 input = let (s:_:mappings) = reverse $ lines input
                  reps = map parseMapping mappings
              in findPathToElectron reps s

day19' :: IO ()
day19' = do
  fileStr <- readFile "resources/day19.txt"
  print $ part2 fileStr
