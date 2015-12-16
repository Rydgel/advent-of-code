{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day16 where

import qualified Data.Map.Lazy as M
import           Data.List
import           Data.Maybe
import           Text.Regex.PCRE.Heavy

{- Day 16: Aunt Sue -}

type Sue = M.Map String Int

tickerTape :: Sue
tickerTape = M.fromList [("children", 3)
                        ,("cats", 7)
                        ,("samoyeds", 2)
                        ,("pomeranians", 3)
                        ,("akitas", 0)
                        ,("vizslas", 0)
                        ,("goldfish", 5)
                        ,("trees", 3)
                        ,("cars", 2)
                        ,("perfumes", 1)
                        ]

parseSue :: String -> Sue
parseSue = M.fromList . map (pair . snd) . scan [re|(\w+): (\d+)|]
  where pair [thing, count] = (thing, read count)
        pair _              = error "parsing error"

matchingSueGeneral :: (Sue -> Sue -> Bool) -> Sue -> [Sue] -> Int
matchingSueGeneral p tape = succ . fromJust . findIndex (p tape)

matchingSue :: Sue -> [Sue] -> Int
matchingSue = matchingSueGeneral $ flip M.isSubmapOf

day16 :: IO ()
day16 = do
  fileStr <- readFile "resources/day16.txt"
  print $ matchingSue tickerTape $ map parseSue $ lines fileStr

{- Part Two -}

isMatching :: Sue -> Sue -> Bool
isMatching tape sue =
  all (\(op, w) -> M.isSubmapOfBy op w tape) [((<), xs), ((>), ys), ((==), zs)]
  where
    isElem xss k _ = k `elem` xss
    (xs, remaining) = M.partitionWithKey (isElem ["pomeranians", "goldfish"]) sue
    (ys, zs) = M.partitionWithKey (isElem ["cats", "trees"]) remaining

matchingSue' :: Sue -> [Sue] -> Int
matchingSue' = matchingSueGeneral isMatching

day16' :: IO ()
day16' = do
  fileStr <- readFile "resources/day16.txt"
  print $ matchingSue' tickerTape $ map parseSue $ lines fileStr
