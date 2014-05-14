{-
    Tabla de joc și mutările posibile.

    Modulul exportă numai funcțiile enumerate mai jos, ceea ce înseamnă
    că doar acestea vor fi vizibile din alte module. Justificarea vizează
    prevenirea accesului extern la structura obiectelor 'Board', și a eventualei
    coruperi a consistenței interne a acestora.
-}
module Board
    ( Board
    , build
    , rows
    , score
    , initialize
    , placeRandomCell
    , moveUp
    , moveDown
    , moveLeft
    , moveRight
    , isWon
    , isLost
    ) where

import System.Random
import Data.List

{-
    *** TODO ***

    Definiți tipul 'Board', astfel încât să rețină informație despre tabla
    de joc și despre scorul curent.

    Observați că viitorii constructori de date și eventualele câmpuri pe care
    le veți specifica nu vor apărea în lista de funcții exportate de modul
    (vezi explicația de la începutul fișierului).
-}
data Board = Board 
	{ gameBoard :: [[Int]]
	, gameScore :: Int
	} deriving Eq
	
testShowBoard = Board [[2,2,0,4], [8,2,16,2], [4,8,2,4], [2,4,2,16]] 0
{-
	Afiseaza o tabla. Formateaza utilizand show de elementul pe care-l iau din lista. 
	Tabla o sa arate in felul urmator: 
	2 | 2| 0| 4|
	8 | 2| 16| 2|
	4 | 8| 2| 4|
	2 | 4| 2| 16|

-}


instance Show Board where
    show (Board gameBoard gameScore) = gameShow
    	where 	line1 = (foldl (\acc y -> acc ++ (show y) ++ " |") "" (takeElem 0 gameBoard)) ++ "\n"
		line2 = (foldl (\acc y -> acc ++ (show y) ++ " |") "" (takeElem 1 gameBoard)) ++ "\n"
		line3 = (foldl (\acc y -> acc ++ (show y) ++ " |") "" (takeElem 2 gameBoard)) ++ "\n"
		line4 = (foldl (\acc y -> acc ++ (show y) ++ " |") "" (takeElem 3 gameBoard)) ++ "\n"
		gameShow = line1 ++ line2 ++ line3 ++ line4

{-
    Construiește o tablă de joc pe baza unei configurații, furnizate pe linii,
    și a unui scor.
-}
build :: [[Int]] -> Int -> Board
build gameBoard gameScore = Board gameBoard gameScore

{-
    Întoarce configurația tablei de joc.
-}
rows :: Board -> [[Int]]
rows (Board gameBoard gameScore) = gameBoard

{-
    Întoarce scorul curent al tablei de joc.
-}

score :: Board -> Int
score (Board gameBoard gameScore) = gameScore

{-

	Plasează aleator o nouă celulă pe tabla de joc.
	getElems returneaza indicii din lista l, cu valoarea n. 
	L-am folosit pentru a luat indicii pentru '0'.

	replaceNth inlocuieste elementul cu indicele n, cu valoarea newVal

	makepl ia o lista [] si alcatuieste o lista [[]]. In cazul nostru, 
	va fi o lista de 4 a cate 4. 
-}

getElems :: Int -> [[Int]] -> [Int]
getElems n l = findIndices (==n) ( concat l )

replaceNth n newVal (x:xs)
     	| n == 0 = newVal:xs
	| otherwise = x:replaceNth (n-1) newVal xs

makepl [] x = reverse x
makepl l x = makepl (drop 4 l) (((take 4 l) : []) ++ x)

placeRandomCell :: RandomGen g => Board -> g -> (Board, g)
placeRandomCell (Board gameBoard gameScore) g = (Board gameBoard2 gameScore, g)
		-- repl conține o listă cu pozițiile elementelor care sunt 0
	where 	repl = getElems 0 gameBoard
		count = length repl
		(randomN, g1) = next g
		whatTo = repl!!(randomN `mod` count)
		withWhat = 2
		gameBoard3 = (replaceNth whatTo withWhat (concat gameBoard))
		gameBoard2 = makepl gameBoard3 []

{-
    Generează aleator o tablă de joc cu două celule ocupate.
-}

initialize :: RandomGen g => g -> (Board, g)
initialize gen = let origBoard = (replicate 4 (replicate 4 0))
	in placeRandomCell (fst (placeRandomCell (Board origBoard 0) gen)) gen

{-
    	Cele patru mutări posibile: stânga, dreapta, sus, jos.
	takeElem: va lua câte un element dintr-o [[]]
	
	take1: ia o lista si verifica daca elementele de la inceput sunt 0
	daca sunt 0, intoarce lista. de ex: [0, 0, 2, 4] -> [2, 4, 0, 0]

	take2: ia cate 2 elemente dintr-o lista. daca x == y si ambele sunt diferite de 0
	atunci le merge-uieste intr-o noua lista + restul listei. Din pacate, nu adauga
	0-urile la sfarsit asa ca am mai facut o noua functie care-mi adauga 0-uri la sf
	listei. Se numeste addZeros. 

	takeScore1: ia cate 2 elemente dintr-o lista si daca sunt egale, le insumeaza.
	
	processRow se foloseste de toate functiile de mai sus pentru a procesa o lista [].

-}
takeElem :: Int -> [[Int]] -> [Int]
takeElem n l = l!!n

take1 :: [Int] -> [Int]
take1 [] = []
take1 [x] = [x]
take1 (x:xs) = if (x==0) then (take1 (reverse (0:(reverse xs)))) else (x:xs)

take2 :: [Int] -> [Int]
take2 [] = []
take2 [x] = [x]
take2 (x:y:xs) = if (x == y) && (x /= 0 && y /= 0) then (x+y):(take2 xs) else x:(take2 (y:xs))

addZeros lst n = lst ++ (take (n - (length lst)) [0,0..])

takeScore1 :: [Int] -> Int
takeScore1 [] = 0
takeScore1 [x] = 0
takeScore1 (x:y:xs) = if x == y then (x+y) + (takeScore1 xs) else (takeScore1 (y:xs))

processRow :: [Int] -> ([Int], Int)
processRow l = (l1, score)
	where 	l2 = take2 l
		l3 = addZeros l2 4
		l1 = take1 l3
		score = takeScore1 l

moveLeft :: Board -> Board
moveLeft (Board gameBoard gameScore) = (Board gameBoard2 gameScore2)
		where 	(firstRow, firstScore) = processRow (takeElem 0 gameBoard)
			(secondRow, secondScore) = processRow (takeElem 1 gameBoard)
			(thirdRow, thirdScore) = processRow (takeElem 2 gameBoard)
			(fourthRow, fourthScore) = processRow (takeElem 3 gameBoard)
			gameBoard2 = firstRow:secondRow:thirdRow:fourthRow:[]
			gameScore2 = firstScore + secondScore + thirdScore + fourthScore

moveUp :: Board -> Board
moveUp (Board gameBoard gameScore) = (Board (transpose gameBoard2) gameScore2)
		where (Board gameBoard2	gameScore2) = moveLeft (Board (transpose gameBoard) gameScore)

moveRight :: Board -> Board
moveRight (Board gameBoard gameScore) = (Board (map reverse gameBoard2) gameScore2)
		where (Board gameBoard2 gameScore2) = moveLeft (Board (map reverse gameBoard) gameScore)

moveDown :: Board -> Board
moveDown (Board gameBoard gameScore) = (Board (transpose (map reverse gameBoard2)) gameScore2)
		where (Board gameBoard2 gameScore2) = moveLeft (Board (map reverse (transpose gameBoard)) gameScore)

{-
    Întoarce 'True' dacă tabla conține o configurație câștigătoare,
    i.e. există cel puțin o celulă cu 2048.
-}
isWon :: Board -> Bool
isWon (Board gameBoard gameScore) = if (length (getElems 2048 gameBoard)) >=1 
						then True
						else False

{- 
 	primeste o functie (moveLeft/moveRight/moveUp/moveDown) si o aplica peste un board.
	Daca score-ul se modifica, inseamna ca miscarea a fost valida. Este utila in verificarea cand 
	sunt ocupate toate casutele, nu sunt 0 si vrem sa vedem daca mai este o miscare posibila. 
 -}
itsMove :: (Board->Board) -> Board -> Bool
itsMove f (Board gameBoard gameScore)
		| gameScore == gameScore2 = False
		| gameScore /= gameScore2 = True
		where (Board gameBoard2 gameScore2) = f (Board gameBoard gameScore)
{-
    Întoarce 'True' dacă tabla conține o configurație în care jucătorul pierde,
    i.e. nu există nicio celulă liberă, și nici nu există celule vecine egale,
    pe orizontală sau verticală.
-}

isLost :: Board -> Bool
isLost (Board gameBoard gameScore) = 	if ((length (getElems 0 gameBoard)) == 0) then 
					(
						if (itsMove moveLeft (Board gameBoard gameScore)) == True then False
					else
						if (itsMove moveRight (Board gameBoard gameScore)) == True then False
					else
						if (itsMove moveUp (Board gameBoard gameScore)) == True then False
					else
						if (itsMove moveDown (Board gameBoard gameScore)) == True then False
					else
						True
					)
					else False
