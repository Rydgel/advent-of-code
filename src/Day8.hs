module Day8 where

import Data.Char

-- todo maybe int
readHex :: Char -> Int
readHex c
    | isDigit c            = fromEnum c - fromEnum '0'
    | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a' + 10
    | c >= 'A' && c <= 'F' = fromEnum c - fromEnum 'A' + 10
readHex _ = error "parse error"

parse :: String -> String
parse [] = []
parse ('\\':'"':xs) = '"' : parse xs
parse ('\\':'\\':xs) = '\\' : parse xs
parse ('\\':'x':a:b:xs) = toEnum (readHex a * 16 + readHex b) : parse xs
parse (x:xs) = x : parse xs

parseLine :: String -> String
parseLine = parse . init . tail

part1 :: String -> Int
part1 input =
    length (concat (lines input)) -
    length (concatMap parseLine (lines input))

part2 :: String -> Int
part2 input =
    length (concatMap show (lines input)) -
    length (concat (lines input))

day8 :: IO ()
day8 = putStrLn "test"
