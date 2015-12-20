{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import qualified Data.Text as T
import           Data.Attoparsec.Text
import           Data.Bits
import           Control.Applicative
import qualified Data.HashMap.Strict as H
import           Data.Function.Memoize (memoize)
import           Data.Either

{- Day 7: Some Assembly Required -}

type Instruction = (String, Node)

data Atom = Value Int | Var String deriving (Show)

data Node = Singleton Atom
          | Not Atom
          | And Atom Atom
          | Or Atom Atom
          | LShift Atom Atom
          | RShift Atom Atom deriving (Show)

type Memory = H.HashMap String Node

-- Parsing {{{

parseLine :: Parser Instruction
parseLine = flip (,) <$> parseNode <* string " -> " <*> many1 letter

parseNode :: Parser Node
parseNode = parseNot
        <|> parseAnd
        <|> parseOr
        <|> parseLShift
        <|> parseRShift
        <|> parseSingleton

parseValue :: Parser Atom
parseValue = Value . read <$> many1 digit

parseVar :: Parser Atom
parseVar = Var <$> many1 letter

parseAtom :: Parser Atom
parseAtom = parseValue <|> parseVar

parseSingleton :: Parser Node
parseSingleton = Singleton <$> parseAtom

parseNot :: Parser Node
parseNot = Not <$> (string "NOT " *> parseAtom)

parseAnd :: Parser Node
parseAnd = And <$> parseAtom <* string " AND " <*> parseAtom

parseOr :: Parser Node
parseOr = Or <$> parseAtom <* string " OR " <*> parseAtom

parseLShift :: Parser Node
parseLShift = LShift <$> parseAtom <* string " LSHIFT " <*> parseAtom

parseRShift :: Parser Node
parseRShift = RShift <$> parseAtom <* string " RSHIFT " <*> parseAtom

-- }}}

-- Evaluating {{{

readData :: [T.Text] -> Memory
readData mappings = H.fromList . rights $ map (parseOnly parseLine) mappings

eval :: Memory -> String -> Int
eval m = me
  where e :: String -> Int
        e k = case m H.! k of
                (Singleton a)  -> getAtom a
                (Not a)        -> complement $ getAtom a
                (And a1 a2)    -> getAtom a1 .&. getAtom a2
                (Or a1 a2)     -> getAtom a1 .|. getAtom a2
                (LShift a1 a2) -> getAtom a1 `shiftL` getAtom a2
                (RShift a1 a2) -> getAtom a1 `shiftR` getAtom a2
        me = memoize e
        getAtom (Value i) = i
        getAtom (Var s)   = me s

-- }}}

day7 :: IO ()
day7 = do
  fileStr <- readFile "resources/day7.txt"
  print . (`eval` "a") . readData . T.lines $ T.pack fileStr

{- Part Two -}

day7' :: IO ()
day7' = do
  fileStr <- readFile "resources/day7.txt"
  let wiring = readData $ T.lines $ T.pack fileStr
      ans1 = eval wiring "a"
      wiring' = H.insert "b" (Singleton $ Value ans1) wiring
  print $ eval wiring' "a"
