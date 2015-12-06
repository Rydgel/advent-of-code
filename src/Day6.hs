{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Day6 where

import           Data.Array.Repa ((:.) (..))
import qualified Data.Array.Repa as R
import           Data.Attoparsec.ByteString.Char8
import           Control.Applicative
import qualified Data.ByteString.Char8 as B

{- Day 6: Probably a Fire Hazard -}

-- Grid {{{

type Grid = R.Array R.D R.DIM2 Int
type Coord = (Int,Int)
data Range = Range Coord Coord deriving (Show)
data Instruction = TurnOff {_range :: Range}
                 | TurnOn {_range :: Range}
                 | Toggle {_range :: Range}
                 deriving (Show)

gridSize :: Int
gridSize = 1000

gridShape :: R.Z :. Int :. Int
gridShape = R.Z :. gridSize :. gridSize

initGrid :: Grid
initGrid = R.fromFunction gridShape $ const 0

-- Yes I need an unboxed type with Repa so no ADTs
toggle :: Int -> Int
toggle 1 = 0
toggle 0 = 1
toggle n = n

-- Kind of slow, but this lib can't work with a specific range
-- I have to traverse everything
-- It take 2s to do day6 but it should take a lot less like (40ms)
workOnRange :: (Int -> Int) -> Range -> Grid -> Grid
workOnRange fun (Range (x1,y1) (x2,y2)) g = R.traverse g id f
  where f a r@(R.Z :. i :. j) = workOn i j $ a r
        workOn i' j' n
          | i' >= x1 && i' <= x2 && j' >= y1 && j' <= y2 = fun n
          | otherwise = n
{-# INLINE workOnRange #-}

turnOnRange :: Range -> Grid -> Grid
turnOnRange = workOnRange $ const 1

turnOffRange :: Range -> Grid -> Grid
turnOffRange = workOnRange $ const 0

toggleRange :: Range -> Grid -> Grid
toggleRange = workOnRange toggle

countLight :: Grid -> IO Int
countLight = R.sumAllP

-- }}}

-- Parsing {{{

action :: Parser (Range -> Instruction)
action = turnOn <|> turnOff <|> toggleL
  where turnOn  = stringCI "turn on" *> pure TurnOn
        turnOff = stringCI "turn off" *> pure TurnOff
        toggleL = stringCI "toggle" *> pure Toggle

pair :: Parser (Int,Int)
pair = (,) <$> decimal <*> (char ',' *> decimal)

range :: Parser Range
range = Range <$> pair <*> (string " through " *> pair)

instruction :: Parser Instruction
instruction = action <*> (char ' ' *> range)

instructions :: Parser [Instruction]
instructions = many $ instruction <* endOfLine

-- }}}

processInstruction :: Grid -> Instruction -> Grid
processInstruction g (TurnOff r) = turnOffRange r g
processInstruction g (TurnOn r)  = turnOnRange r g
processInstruction g (Toggle r)  = toggleRange r g

processInstructions :: [Instruction] -> Grid
processInstructions = foldl processInstruction initGrid

day6 :: IO ()
day6 = do
  fileStr <- B.readFile "resources/day6.txt"
  case parseOnly instructions fileStr of
    Left e -> print e -- parsing error
    Right is -> print =<< countLight (processInstructions is)

{- Day Two -}

turnOnRange2 :: Range -> Grid -> Grid
turnOnRange2 = workOnRange (+1)

turnOffRange2 :: Range -> Grid -> Grid
turnOffRange2 = workOnRange $ max 0 . subtract 1

toggleRange2 :: Range -> Grid -> Grid
toggleRange2 = workOnRange (+2)

processInstruction2 :: Grid -> Instruction -> Grid
processInstruction2 g (TurnOff r) = turnOffRange2 r g
processInstruction2 g (TurnOn r)  = turnOnRange2 r g
processInstruction2 g (Toggle r)  = toggleRange2 r g

processInstructions2 :: [Instruction] -> Grid
processInstructions2 = foldl processInstruction2 initGrid

day6' :: IO ()
day6' = do
  fileStr <- B.readFile "resources/day6.txt"
  case parseOnly instructions fileStr of
    Left e -> print e -- parsing error
    Right is -> print =<< countLight (processInstructions2 is)
