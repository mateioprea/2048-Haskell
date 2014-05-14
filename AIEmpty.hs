module AIEmpty where

import Board
import System.Random
import Interactive
import Data.List
import Data.Ord
{-
    Întoarce tabla rezultată din aplicarea acelei mutări care maximizează
    numărul de celule libere.
-}
getZeros :: Board -> [Int]
getZeros table = filter (==0) (concat (rows table))

maxIndex ::  Ord a => [a] -> Int
maxIndex = fst . maximumBy (comparing snd) . zip [0..]

move :: Board -> Board
move table = table2
	where 	possibleMoves = [moveLeft table, moveRight table, moveUp table, moveDown table]
		elemsZero = map length (map getZeros (possibleMoves))
		getIndexOf = maxIndex elemsZero
		table2 = possibleMoves!!getIndexOf

{-
    Urmărește pas cu pas evoluția jocului, conform strategiei implementate.
-}
userMode :: IO ()
userMode = ai move
