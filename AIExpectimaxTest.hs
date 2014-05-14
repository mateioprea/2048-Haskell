{-# OPTIONS_GHC -F -pgmF htfpp #-}

module AIExpectimaxTest where

import Board
import AIExpectimax
import Interactive
import System.Random
import Data.Char
import Test.Framework

merge :: [[Int]]
merge = [ [ 2, 2, 0, 0 ]
        , [ 2, 2, 2, 0 ]
        , [ 2, 2, 2, 2 ]
        , [ 2, 2, 4, 4 ]
        ]

test_maximize :: IO ()
test_maximize = do
    assertEqual 0 $ maximize id $ expand [] 0
    assertEqual 3 $ maximize id $ expand [const [1, 3]] 0

test_expect :: IO ()
test_expect = do
    assertEqual 0 $ expect id $ expand [] 0
    assertEqual 2 $ expect id $ expand [const [1, 3]] 0

test_pick :: IO ()
test_pick = do
    assertEqual 'c' $ pick (fromIntegral . ord) $ expand [const "bc"] 'a'

test_prune :: IO ()
test_prune = do
    -- Alegeri diferite, în funcție de adâncimea de explorare
    assertEqual 1 $ pick id tree
    assertEqual 2 $ pick id $ prune 1 tree
  where
    tree = expand [const [1, 2], \x -> [5 - x, 6 - x]] 0

test_play :: IO ()
test_play = do
    initialBoard <- getStdRandom initialize
    finalBoard   <- getStdRandom $ play move initialBoard
    print finalBoard
    assertBool $ score finalBoard > 0
