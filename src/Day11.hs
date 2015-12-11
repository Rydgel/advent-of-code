module Day11 where

import           Data.List
import           Control.Applicative

{- Day 11: Corporate Policy -}

input :: String
input = "cqjxjnds"

inc :: Char -> (Bool, String) -> (Bool, String)
inc x (carry, xs)
    | not carry    = (False, x:xs)
    | x == 'z'     = (True, 'a':xs)
    | x `elem` "hnk" = (False, (succ . succ $ x) : xs)
    | otherwise    = (False, succ x : xs)

incStr :: String -> String
incStr = snd . foldr inc (True, [])

has2Pairs :: String -> Bool
has2Pairs = (>= 2) . length . filter ((>= 2) . length) . group

hasStraight :: String -> Bool
hasStraight = any ((>= 3) . length) . group . zipWith ($)
              (scanl' (.) id (repeat pred))

hasNoIOL :: String -> Bool
hasNoIOL s = not ('i' `elem` s || 'o' `elem` s || 'l' `elem` s)

isValid :: String -> Bool
isValid s = has2Pairs s && hasStraight s && hasNoIOL s

getNextPassword :: String -> String
getNextPassword = head . filter isValid . tail . iterate incStr

day11 :: IO ()
day11 = print $ getNextPassword input

{- Part Two -}

input2 :: String
input2 = "cqjxxyzz"

day11' :: IO ()
day11' = print $ getNextPassword input2
