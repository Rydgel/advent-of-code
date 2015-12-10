module Day10 where

import Data.List

{- Day 10: Elves Look, Elves Say -}

lookAndSay :: String -> String
lookAndSay = concatMap (((++) . show . length) <*> (return . head)) . group

day10 :: IO ()
day10 = print . length . (!! 40) . iterate lookAndSay $ "1113222113"

{- Part Two -}

day10' :: IO ()
day10' = print . length . (!! 50) . iterate lookAndSay $ "1113222113"
