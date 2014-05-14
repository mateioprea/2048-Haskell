module Solitary where

import Board
import Interactive

{-
	Jocul solitar al utilizatorului.
-}
userMode :: IO ()
userMode = interactive $ \board -> do
    putStr "Move (w/a/s/d): "
    key <- getLine
    return $ flip ($) board $ case key of
        "w" -> moveUp
        "s" -> moveDown
        "a" -> moveLeft
        "d" -> moveRight