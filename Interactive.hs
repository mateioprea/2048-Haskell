module Interactive where

import Board
import System.Random

{-
    *** TODO ***

    Aplică repetat euristica asupra tablei inițiale până la câștig sau pierdere,
    întorcând tabla cu care jocul s-a terminat.
-}
play :: RandomGen g => (Board -> Board) -> Board -> g -> (Board, g)
play move table g = if (isWon table) then (table, g) else if (isLost table) then (table, g) else (table, g)
{-
    Avansează pas cu pas jocul, pe baza unei strategii primite ca parametru.
    Este folosită atât pentru jocul solitar al utilizatorului,
    cât și pentru urmărirea pas cu pas a jocului bazat pe euristică.
-}
interactive :: (Board -> IO Board) -> IO ()
interactive strategy = getStdRandom initialize >>= loop
  where
    loop :: Board -> IO ()
    loop board = do
        print board
        if isWon board
            then putStrLn "You won!"
            else if isLost board
                then putStrLn "Game over!"
                else do
                    movedBoard <- strategy board
                    -- Dacă mutarea nu a produs o schimbare de configurație
                    -- a tablei, nu se generează o celulă nouă.
                    loop =<< if movedBoard == board
                             then return board
                             else getStdRandom $ placeRandomCell movedBoard

{-
    Jocul solitar al utilizatorului.
-}
solitary :: IO ()
solitary = interactive $ \board -> do
    putStr "Move (w/a/s/d): "
    key <- getLine
    return $ flip ($) board $ case key of
        "w" -> moveUp
        "s" -> moveDown
        "a" -> moveLeft
        "d" -> moveRight

{-
    Urmărește pas cu pas evoluția jocului, pe baza unei euristici.
-}
ai :: (Board -> Board) -> IO ()
ai move = interactive $ \board -> do
    putStr "Press return..."
    _ <- getLine
    return $ move board
