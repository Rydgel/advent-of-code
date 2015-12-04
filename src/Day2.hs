module Day2 where

import Data.List
import Data.List.Split
import Control.Applicative

{- Day 2: I Was Told There Would Be No Math -}

type Sides = (Int, Int, Int)

surface :: Sides -> Int
surface (l,w,h) = 2*l*w + 2*w*h + 2*h*l

smallSurface :: Sides -> Int
smallSurface (l,w,h) = minSurf sorted
  where sorted = sort [l,w,h]
        minSurf (a:b:_) = a*b
        minSurf _       = 0

paperNeeded :: Sides -> Int
paperNeeded = liftA2 (+) surface smallSurface

parseLine :: String -> Sides
parseLine s = tuplify3 $ map (\x -> read x :: Int) $ splitOn "x" s
  where tuplify3 :: [a] -> (a,a,a)
        tuplify3 [x,y,z] = (x,y,z)
        tuplify3 _ = error "can't tuplify3 this"

day2 :: IO ()
day2 = do
  fileStrLn <- readFile "resources/day2.txt"
  print $ foldl (\z s -> z + paperNeeded (parseLine s)) 0 (lines fileStrLn)


{- Part Two -}

ribbonNeeded :: Sides -> Int
ribbonNeeded (l,w,h) = minRibbon sorted
  where sorted = sort [l,w,h]
        minRibbon [a,b,c] = 2*a + 2*b + a*b*c
        minRibbon _       = 0

day2' :: IO ()
day2' = do
  fileStrLn <- readFile "resources/day2.txt"
  print $ foldl (\z s -> z + ribbonNeeded (parseLine s)) 0 (lines fileStrLn)
