module Day3 where

import qualified Data.HashMap as H
import           Control.Arrow

type Coord = (Int,Int)
type Grid = H.Map Coord Int
type Santa = (Coord,Grid)
type RobotSanta = (Coord,Grid)

initGrid :: Grid
initGrid = H.singleton (0,0) 1

initSanta :: Santa
initSanta = ((0,0), initGrid)

moveUp :: Coord -> Coord
moveUp (a,b) = (a,b+1)

moveDown :: Coord -> Coord
moveDown (a,b) = (a,b-1)

moveRight :: Coord -> Coord
moveRight (a,b) = (a+1,b)

moveLeft :: Coord -> Coord
moveLeft (a,b) = (a-1,b)

addPresent :: Grid -> Coord -> Grid
addPresent grid coord = H.insertWith (+) coord 1 grid

move :: Santa -> Char -> Santa
move (coord,grid) '^' = (moveUp coord, addPresent grid $ moveUp coord)
move (coord,grid) 'v' = (moveDown coord, addPresent grid $ moveDown coord)
move (coord,grid) '>' = (moveRight coord, addPresent grid $ moveRight coord)
move (coord,grid) '<' = (moveLeft coord, addPresent grid $ moveLeft coord)
move (coord,grid)  _  = (coord, grid)

parseSanta :: String -> Santa
parseSanta = foldl move initSanta

countHouseWithPresent :: Grid -> Int
countHouseWithPresent = H.size . H.filter (>=1)

day3 :: IO ()
day3 = do
  fileStr <- readFile "resources/day3.txt"
  print $ countHouseWithPresent $ snd $ parseSanta fileStr

{- Part Two -}

initRobot :: RobotSanta
initRobot = ((0,0), initGrid)

parseRobot :: String -> RobotSanta
parseRobot = foldl move initRobot

split :: String -> (String,String)
split [] = ([], [])
split [x] = ([x], [])
split (x:y:xs) = (x:xp, y:yp)
  where (xp, yp) = split xs

bothPath :: (String,String) -> (Grid,Grid)
bothPath = (snd . parseSanta) *** (snd . parseRobot)

mergeGrid :: (Grid,Grid) -> Grid
mergeGrid = uncurry $ H.unionWith (+)

day3' :: IO ()
day3' = do
  fileStr <- readFile "resources/day3.txt"
  print $ countHouseWithPresent $ mergeGrid $ bothPath $ split fileStr
