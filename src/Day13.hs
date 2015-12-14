{-# LANGUAGE OverloadedStrings #-}

module Day13 where

import           Data.List    as L
import qualified Data.HashMap as M
import           Data.Ord
import qualified Data.Text    as T
import           Data.Attoparsec.Text
import           Data.Either

{- Day 13: Knights of the Dinner Table -}

type SeatPair = (String, String)
type SeatTriple = (String, String, String)

parseSeat :: Parser (SeatPair, Int)
parseSeat = do
  p <- many1 letter
  _ <- string " would "
  s <- many1 letter
  _ <- space
  x <- decimal
  _ <- string " happiness units by sitting next to "
  n <- many1 letter
  _ <- char '.'
  let v = x * (if s == "gain" then 1 else -1)
  return ((p, n), v)

scoreNeighbors :: M.Map SeatPair Int -> [String] -> Int
scoreNeighbors nm ns = sum $ map s (zip3 ns' (tail ns') (tail $ tail ns'))
    where ns' = L.take (length ns + 2) $ cycle ns
          s (l,x,r) = nm M.! (x,l) + nm M.! (x,r)

day13 :: IO ()
day13 = do
    parseS <- T.lines . T.pack <$> readFile "resources/day13.txt"
    let ns = M.fromList . rights $ map (parseOnly parseSeat) parseS
        ps = permutations . nub . map fst $ M.keys ns
        ss = map (scoreNeighbors ns) ps
    print $ maximumBy (comparing snd) $ zip ps ss

{- Part Two -}

day13' :: IO ()
day13' = do
  parseS <- T.lines . T.pack <$> readFile "resources/day13.txt"
  let ns = rights $ map (parseOnly parseSeat) parseS
      ks = nub . map (fst . fst) $ ns
      me = zip (zip (repeat "Rydgel") ks) (repeat 0)
        ++ zip (zip ks (repeat "Rydgel")) (repeat 0)
      ns' = M.fromList (ns ++ me)
      ps = permutations ("Rydgel" : ks)
      ss = map (scoreNeighbors ns') ps
  print $ maximumBy (comparing snd) $ zip ps ss
