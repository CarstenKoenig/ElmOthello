module AI
    exposing
        ( Level
        , blackAI
        )

import Array exposing (Array)
import MiniMax exposing (..)
import Othello exposing (..)


type alias Level =
    Int


blackAI : Level -> Board -> Maybe Move
blackAI level board =
    let
        ( mv, _ ) =
            findMax othelloProblem level board
    in
        mv


othelloProblem : Problem Board Move
othelloProblem =
    Problem heuristic moves applyMove


moves : PlayerType -> Board -> Maybe ( PlayerType, List Move )
moves playerType board =
    case validNextMoves board (toStone playerType) of
        NoValidMoves ->
            Nothing

        ValidMoves player moves ->
            Just ( toPlayerType player, toMoveList moves )


heuristic : Board -> Int
heuristic (Board rows) =
    valueRows coordValues rows


coordValues : List (List Int)
coordValues =
    [ [ 10, -5, 6, 5, 5, 6, -5, 10 ]
    , [ -5, -8, -4, -4, -4, -4, -8, -5 ]
    , [ 6, -4, 2, 3, 3, 2, -4, 6 ]
    , [ 5, -4, 3, 4, 4, 3, -4, 6 ]
    , [ 5, -4, 3, 4, 4, 3, -4, 6 ]
    , [ 6, -4, 2, 3, 3, 2, -4, 6 ]
    , [ -5, -8, -4, -4, -4, -4, -8, -5 ]
    , [ 10, -5, 6, 5, 5, 6, -5, 10 ]
    ]


valueRows : List (List Int) -> Array (Array Cell) -> Int
valueRows factors rows =
    List.map2 valueRow factors (Array.toList rows)
        |> List.sum


valueRow : List Int -> Array Cell -> Int
valueRow factors cells =
    List.map2 valueCell factors (Array.toList cells)
        |> List.sum


valueCell : Int -> Cell -> Int
valueCell factor cell =
    case cell of
        Empty ->
            0

        Occupied Black ->
            factor

        Occupied White ->
            -factor


toStone : PlayerType -> Stone
toStone playerType =
    case playerType of
        Maximizing ->
            Black

        Minimizing ->
            White


toPlayerType : Stone -> PlayerType
toPlayerType stone =
    case stone of
        Black ->
            Maximizing

        White ->
            Minimizing
