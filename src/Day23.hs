{-# LANGUAGE OverloadedStrings #-}

module Day23 where

import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (pack)
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V
import Data.Either

{- Day 23: Opening the Turing Lock -}

data Instruction = Cmd { inst :: String
                       , reg :: String
                       , off :: String
                       } deriving (Show)

type Registers = M.HashMap String Int

{- Parser -}

instr :: Parser Instruction
instr = do
  i <- many1 letter_ascii <* space
  r <- many1 $ notChar ','
  o <- option "" $ char ',' *> space *> many1 (notChar ' ')
  return $ Cmd i r o

signedInt :: Parser Int
signedInt = signed decimal

cmd :: Registers -> Instruction -> Registers
cmd mem (Cmd i r o)
  | i == "hlf"           = M.adjust (`div` 2) r mem'
  | i == "tpl"           = M.adjust (*3) r mem'
  | i == "inc"           = M.adjust (+1) r mem'
  | i == "jmp"           = M.adjust (+r') "addr" mem
  | i == "jie" && even v = M.adjust (+o') "addr" mem
  | i == "jio" && v == 1 = M.adjust (+o') "addr" mem
  | otherwise            = mem'
  where mem' = M.adjust (+1) "addr" mem
        v    = mem M.! r
        (Right r') = parseOnly signedInt $ pack r
        (Right o') = parseOnly signedInt $ pack o

run :: Registers -> V.Vector Instruction -> Registers
run mem instrs
  | 0 <= i && i < V.length instrs = run (cmd mem $ instrs V.! i) instrs
  | otherwise                     = mem
  where i = mem M.! "addr"

execute :: Registers -> String -> Int
execute mem = (M.! "b") . run mem . V.fromList . rights . map (parseOnly instr . pack) . lines

day23 :: IO ()
day23 = do
  fileStr <- readFile "resources/day23.txt"
  let mem = M.fromList [("a", 0), ("b", 0), ("addr", 0)]
  print $ execute mem fileStr

{- Part Two -}

day23' :: IO ()
day23' = do
  fileStr <- readFile "resources/day23.txt"
  let mem = M.fromList [("a", 1), ("b", 0), ("addr", 0)]
  print $ execute mem fileStr
