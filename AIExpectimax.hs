module AIExpectimax where

import Board
import Interactive
import Data.List
import Control.Monad (liftM2)

{-
    *** TODO ***

    Implementați tipul 'Tree a', al arborilor expectimax.
-}
data Tree a = Undefined

{-
    *** TODO ***

    Întoarce tabla rezultată din aplicarea unei mutări alese de o euristică
    în contextul expectimax.
-}
move :: Board -> Board
move = undefined

{-
    *** TODO ***

    Construiește un arbore expectimax (eventual infinit), pornind de la lista
    funcțiilor de generare per nivel și de la rădăcină.

    Pentru generarea succesorilor nivelului k, se utilizează funcția
    de pe poziția k din listă. Pentru arbori infiniți, această listă va fi
    de asemenea infinită.
-}
expand :: [a -> [a]] -> a -> Tree a
expand = undefined

{-
    *** TODO ***

    Limitează un arbore la numărul de niveluri dat ca parametru.
-}
prune :: Int -> Tree a -> Tree a
prune = undefined

{-
    *** TODO ***

    Determină valoarea expectimax a unui nod MAX. Funcția de evaluare statică
    este dată ca parametru.
-}
maximize :: (a -> Float) -> Tree a -> Float
maximize = undefined

{-
    *** TODO ***

    Determină valoarea expectimax a unui nod ȘANSĂ. Funcția de evaluare statică
    este dată ca parametru.
-}
expect :: (a -> Float) -> Tree a -> Float
expect = undefined

{-
    *** TODO ***

    Întoarce cheia copilului rădăcinii arborelui expectimax, ales
    în conformitate cu principiul algoritmului. Funcția de evaluare statică
    este dată ca parametru.
-}
pick :: (a -> Float) -> Tree a -> a
pick = undefined

{-
    Calculează media unei liste.
-}
average :: [Float] -> Float
average = liftM2 (/) sum genericLength

{-
    Urmărește pas cu pas evoluția jocului, conform strategiei implementate.
-}
userMode :: IO ()
userMode = ai move