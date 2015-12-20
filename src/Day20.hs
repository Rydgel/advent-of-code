module Day20 where

import qualified Data.Vector.Unboxed.Mutable as V
import           Control.Monad
import           Control.Monad.Trans.Either
import           Control.Monad.IO.Class

{- Day 20: Infinite Elves and Infinite Houses -}

input :: Int
input = 29000000

-- Fast but dayum ugly.
answer :: Int -> Int -> IO (Either Int ())
answer m limit = do
  let d = input `div` m
  vect <- V.replicate d 0
  runEitherT $ forM_ [1..d] $ \i -> do
    let b = min (d `div` i) limit
    _ <- runEitherT $ forM_ [1..b] $ \j -> do
      v <- liftIO $ V.read vect (j*i-1)
      liftIO $ V.write vect (j*i-1) (v+i*m)
    v' <- liftIO $ V.read vect (i-1)
    when (v' >= input) $ left i


day20 :: IO ()
day20 = do
  eitherM <- answer 10 (maxBound :: Int)
  case eitherM of
    (Left i) -> print i
    _        -> error "Unexpected return"

{- Part Two -}

day20' :: IO ()
day20' = do
  eitherM <- answer 11 50
  case eitherM of
    (Left i) -> print i
    _        -> error "Unexpected return"
