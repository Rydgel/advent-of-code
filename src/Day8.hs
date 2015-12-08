module Day8 where

{- Day 8: Matchsticks -}

count :: String -> Int
count [] = 0
count ('\\':'"':xs) = 1 + count xs
count ('\\':'\\':xs) = 1 + count xs
count ('\\':'x':_:_:xs) = 1 + count xs
count (_:xs) = 1 + count xs

countLine :: String -> Int
countLine = count . init . tail

part1 :: String -> Int
part1 input =
    length (concat (lines input)) -
    sum (map countLine (lines input))

day8 :: IO ()
day8 = do
  fileStr <- readFile "resources/day8.txt"
  print $ part1 fileStr

{- Part Two -}

part2 :: String -> Int
part2 input =
    length (concatMap show (lines input)) -
    length (concat (lines input))

day8' :: IO ()
day8' = do
  fileStr <- readFile "resources/day8.txt"
  print $ part2 fileStr
