module Day1 where

{- Day 1: Not Quite Lisp -}

getFloors :: String -> Int
getFloors = foldl getCharValue 0

getCharValue :: Int -> Char -> Int
getCharValue z '(' = z+1
getCharValue z ')' = z-1
getCharValue z  _  = z

day1 :: IO ()
day1 = do
  fileStr <- readFile "resources/day1.txt"
  print $ getFloors fileStr

{- Part Two -}

getAllFloors :: String -> [Int]
getAllFloors = scanl getCharValue 0

findFirstSpecificFloor :: Int -> Int -> [Int] -> Int
findFirstSpecificFloor n z []     = z
findFirstSpecificFloor n z (f:fs) = if f == n
                                    then z
                                    else findFirstSpecificFloor n (z+1) fs

day1' :: IO ()
day1' = do
  fileStr <- readFile "resources/day1.txt"
  print $ findFirstSpecificFloor (-1) 0 $ getAllFloors fileStr
