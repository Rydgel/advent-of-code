module Day9 where

import qualified Data.HashMap.Strict as H
import Data.List
import Data.Ord

{- Day 9: All in a Single Night -}

type Dist = Int
type Node = String
type Path = (Node, Node)
type PathMap = H.HashMap Path Dist

readPaths :: String -> PathMap
readPaths = H.fromList . concatMap (f . words) . lines
    where f [a,_,b,_,d] = [((a,b),read d),((b,a),read d)]
          f _           = error "parsing error"

getNodes :: PathMap -> [Node]
getNodes = nub . map fst . H.keys

getPaths :: [Node] -> [Path]
getPaths = zip <*> tail

getPathDist :: PathMap -> Path -> Dist
getPathDist = (H.!)

getPathsDist :: PathMap -> [Path] -> Dist
getPathsDist = (sum .) . map . getPathDist

getRoutes :: String -> [([Path], Dist)]
getRoutes s = let pathMap = readPaths s
                  nodes = getNodes pathMap
                  perms = permutations nodes
                  paths = map getPaths perms
                  dists = map (getPathsDist pathMap) paths
               in zip paths dists

day9 :: IO ()
day9 = do
    fileStr <- readFile "resources/day9.txt"
    print $ minimumBy (comparing snd) $ getRoutes fileStr

{- Part Two -}

day9' :: IO ()
day9' = do
  fileStr <- readFile "resources/day9.txt"
  print $ maximumBy (comparing snd) $ getRoutes fileStr
