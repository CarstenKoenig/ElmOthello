module AI
    exposing
        ( Level
        , blackAI
        )

import Othello exposing (..)
import MiniMax exposing (..)


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
heuristic board =
    countStones board Black - countStones board White


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
