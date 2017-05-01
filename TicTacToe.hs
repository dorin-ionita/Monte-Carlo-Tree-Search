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

data Board = BoardConstructor { player_to_move :: Player
                                , square_1 :: Maybe Player
                                , square_2 :: Maybe Player
                                , square_3 :: Maybe Player
                                , square_4 :: Maybe Player
                                , square_5 :: Maybe Player
                                , square_6 :: Maybe Player
                                , square_7 :: Maybe Player
                                , square_8 :: Maybe Player
                                , square_9 :: Maybe Player
                                } 
    
    deriving Eq

getSquare1 :: Board -> Maybe Player
getSquare1 (BoardConstructor {square_1 = sq}) = sq

getSquare2 :: Board -> Maybe Player
getSquare2 (BoardConstructor {square_2 = sq}) = sq

getSquare3 :: Board -> Maybe Player
getSquare3 (BoardConstructor {square_3 = sq}) = sq

getSquare4 :: Board -> Maybe Player
getSquare4 (BoardConstructor {square_4 = sq}) = sq

getSquare5 :: Board -> Maybe Player
getSquare5 (BoardConstructor {square_5 = sq}) = sq

getSquare6 :: Board -> Maybe Player
getSquare6 (BoardConstructor {square_6 = sq}) = sq

getSquare7 :: Board -> Maybe Player
getSquare7 (BoardConstructor {square_7 = sq}) = sq

getSquare8 :: Board -> Maybe Player
getSquare8 (BoardConstructor {square_8 = sq}) = sq

getSquare9 :: Board -> Maybe Player
getSquare9 (BoardConstructor {square_9 = sq}) = sq

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
boardConfiguration board = [getSquare7 $ board,
                            getSquare9 $ board,
                            getSquare9 $ board,
                            getSquare4 $ board,
                            getSquare5 $ board,
                            getSquare6 $ board,
                            getSquare1 $ board,
                            getSquare2 $ board,
                            getSquare3 $ board]

{-
    *** TODO ***

    Întoarce jucătorul care urmează să mute.
-}
boardPlayer :: Board -> Player
boardPlayer (BoardConstructor {player_to_move = pl}) = pl

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
