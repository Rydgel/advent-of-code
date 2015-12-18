module Day18 where

import qualified Data.HashMap as M
import           Data.Maybe

{- Day 18: Like a GIF For Your Yard -}

type Coord = (Int,Int)
data LightStatus = LightOpen | LightClosed deriving (Show, Eq)
type Grid = M.Map Coord LightStatus

parseGrid :: String -> Grid
parseGrid = fst . foldl parseChar (M.empty, (0,0))
  where parseChar (z,(_,y))  '\n' = (z, (0,y+1))
        parseChar (z,c@(x,y)) '#' = (M.insert c LightOpen z, (x+1,y))
        parseChar (z,c@(x,y)) '.' = (M.insert c LightClosed z, (x+1,y))
        parseChar _ _ = error "parsing error"

countNeighborsOn :: Coord -> Grid -> Int
countNeighborsOn (x,y) g = length $ filter (==LightOpen)
  [ fromMaybe LightClosed $ M.lookup (x-1,y-1) g
  , fromMaybe LightClosed $ M.lookup (x,y-1) g
  , fromMaybe LightClosed $ M.lookup (x+1,y-1) g
  , fromMaybe LightClosed $ M.lookup (x+1,y) g
  , fromMaybe LightClosed $ M.lookup (x+1,y+1) g
  , fromMaybe LightClosed $ M.lookup (x,y+1) g
  , fromMaybe LightClosed $ M.lookup (x-1,y+1) g
  , fromMaybe LightClosed $ M.lookup (x-1,y) g
  ]

step :: Grid -> Grid
step g = M.mapWithKey f g
  where f c LightOpen   = case countNeighborsOn c g of
                            2 -> LightOpen
                            3 -> LightOpen
                            _ -> LightClosed
        f c LightClosed = case countNeighborsOn c g of
                            3 -> LightOpen
                            _ -> LightClosed

stepX :: Int -> Grid -> Grid
stepX n g = iterate step g !! n

countLights :: Grid -> Int
countLights = M.size . M.filter (==LightOpen)

day18 :: IO ()
day18 = do
  fileStr <- readFile "resources/day18.txt"
  print $ countLights $ stepX 100 $ parseGrid fileStr

{- Part Two -}

cornerify :: Grid -> Grid
cornerify = M.adjust (const LightOpen) (0,0)
          . M.adjust (const LightOpen) (0,99)
          . M.adjust (const LightOpen) (99,0)
          . M.adjust (const LightOpen) (99,99)

stepX' :: Int -> Grid -> Grid
stepX' n g = iterate (cornerify . step) (cornerify g) !! n

day18' :: IO ()
day18' = do
  fileStr <- readFile "resources/day18.txt"
  print $ countLights $ stepX' 100 $ parseGrid fileStr
