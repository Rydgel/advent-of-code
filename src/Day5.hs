module Day5 where

import Control.Arrow
import Data.List
import Control.Lens

{- Day 5: Doesn't He Have Intern-Elves For This? -}

isNice :: String -> Bool
isNice s = not (any (`isInfixOf` s) ["ab", "cd", "pq", "xy"]) &&
           ((>=3) . length $ filter (`elem` "aeiou") s) &&
           any ((>=2) . length) (group s)

day5 :: IO ()
day5 = do
  fileStr <- readFile "resources/day5.txt"
  print $ length $ filter isNice $ lines fileStr

{- Part Two -}

everyOther :: [a] -> ([a], [a])
everyOther (x:y:xs) = (x:) *** (y:) $ everyOther xs
everyOther (x:xs) = first (x:) $ everyOther xs
everyOther [] = ([], [])

isNice2 :: String -> Bool
isNice2 s = g s && f s
  where f s' = let (a, b) = everyOther s'
               in any (\x -> or . zipWith (==) x $ tail x) [a, b]
        g s' = or [ any ([a, b] `isInfixOf`) [c, d]
                  | (i, a, b) <- zip3 [0..] s' $ tail s'
                  , let (c, d) = _2 %~ drop 2 $ splitAt i s']

day5' :: IO ()
day5' = do
  fileStr <- readFile "resources/day5.txt"
  print $ length $ filter isNice2 $ lines fileStr
