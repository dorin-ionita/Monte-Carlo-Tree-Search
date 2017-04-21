{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, TupleSections #-}

module TicTacToe where

import MCTS
import GameState

import System.Random

{-
    Tipul celulelor (1-9)
-}
type Cell = Int

{-
    Tipul jucătorilor
-}
data Player = X | O
    deriving (Eq, Enum, Show)

{-
    Întoarce celălalt jucător.
-}
otherPlayer :: Player -> Player
otherPlayer X = O
otherPlayer O = X

{-
    *** TODO ***

    Tipul stării jocului. Ar trebui să conțină informații despre tablă
    și despre jucătorul care urmează să mute.
-}
data Board = UndefinedBoard
    deriving Eq

{-
    *** TODO ***

    Întoarce lista conținuturilor celulelor, unde celule libere
    sunt reprezentate de `Nothing`.

    Ordinea celulelor este următoarea:

    789
    456
    123
-}
boardConfiguration :: Board -> [Maybe Player]
boardConfiguration = undefined

{-
    *** TODO ***

    Întoarce jucătorul care urmează să mute.
-}
boardPlayer :: Board -> Player
boardPlayer = undefined

{-
    *** TODO ***

    Instanțiați clasa `Show` cu tipul `Board`.
-}
instance Show Board where
    show = undefined

{-
    *** TODO ***

    Instanțiați clasa `GameBoard` cu tipurile `Board` și `Cell`.
-}
instance GameState Board Cell where
    -- playerIndex :: Board -> Int
    playerIndex = undefined

    -- maxPlayers :: Board -> Int
    maxPlayers = undefined

    -- successors :: Board -> [(Cell, Board)]
    successors = undefined

    -- outcome :: Board -> Outcome
    outcome = undefined

{-
    *** TODO ***

    Tabla inițială de joc. X mută primul.
-}
initialBoard :: Board
initialBoard = undefined

{-
    *** TODO ***

    Mută în celula dată ca parametru, în funcție de jucătorul aflat la rând,
    și schimbă jucătorul curent.

    Ordinea celulelor este explicată la funcția `boardConfiguration`.
-}
place :: Cell -> Board -> Maybe Board
place = undefined

{-
    *** TODO ***

    Alege o mutare pornind din starea curentă.

    Utilizați `choose` din modulul `MCTS`, cu un număr dorit de iterații
    ale algoritmului.

    Pentru a juca contra calculatorului, rulați din modulul `Interactive`:

    > humanVsAI step
-}
step :: Board -> StdGen -> (Cell, Board)
step = undefined
