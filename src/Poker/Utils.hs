module Poker.Utils where

------------------------------------------------------------------------------
import Control.Monad.State hiding (state)

------------------------------------------------------------------------------
import Poker.Types

------------------------------------------------------------------------------
import Control.Monad
import Data.Array.IO
import System.Random

shuffle :: [a] -> IO [a]
shuffle xs = do
  ar <- newArray n xs
  forM [1 .. n] $ \i -> do
    j <- randomRIO (i, n)
    vi <- readArray ar i
    vj <- readArray ar j
    writeArray ar j vi
    return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs = newListArray (1, n) xs
