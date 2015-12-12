{-# LANGUAGE OverloadedStrings #-}

module Day12 where

import           Data.Aeson
import qualified Data.ByteString.Lazy  as BL
import qualified Data.HashMap.Strict   as M
import           Data.Maybe
import           Data.Scientific
import           Data.Text
import qualified Data.Vector           as V

{- Day 12: JSAbacusFramework.io -}

numberFold :: (M.HashMap Text Value -> Bool) -> Scientific -> Value -> Scientific
numberFold f s (Object o) | f o = M.foldl' (numberFold f) s o
numberFold f s (Array a) = V.foldl' (numberFold f) s a
numberFold _ s (Number b) = s + b
numberFold _ s _ = s

input :: IO Value
input = fromJust <$> decode <$> BL.readFile "resources/day12.txt"

day12 :: IO ()
day12 = print =<< numberFold (const True) 0 <$> input

{- Part Two -}

day12' :: IO ()
day12' = print =<< numberFold (notElem "red" . M.elems) 0 <$> input
