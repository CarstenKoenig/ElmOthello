module Othello
    exposing
        ( Board
        , Cell(..)
        , Stone(..)
        , Coord
        , Move
        , Moves
        , NextMoves(..)
        , startBoard
        , initialValidMoves
        , otherColor
        , cellAt
        , countStones
        , applyMove
        , moveAt
        , emptyMoves
        , validNextMoves
        )

import Array exposing (Array)
import Array as Array
import Dict exposing (Dict)
import Dict as Dict


type Board
    = Board (Array (Array Cell))


startBoard : Board
startBoard =
    [ List.repeat 8 Empty
    , List.repeat 8 Empty
    , List.repeat 8 Empty
    , [ Empty, Empty, Empty, Occupied Black, Occupied White, Empty, Empty, Empty ]
    , [ Empty, Empty, Empty, Occupied White, Occupied Black, Empty, Empty, Empty ]
    , List.repeat 8 Empty
    , List.repeat 8 Empty
    , List.repeat 8 Empty
    ]
        |> List.map Array.fromList
        |> Array.fromList
        |> Board


type Cell
    = Empty
    | Occupied Stone


type Stone
    = Black
    | White


otherColor : Stone -> Stone
otherColor stone =
    case stone of
        Black ->
            White

        White ->
            Black


type NextMoves
    = NoValidMoves
    | ValidMoves Stone Moves


type Moves
    = Moves (Dict Coord Move)


type Move
    = Move
        { atCoord : Coord
        , changedCells : List Coord
        , player : Stone
        }


type alias Coord =
    ( Int, Int )


initialValidMoves : Moves
initialValidMoves =
    validMoves startBoard White


cellAt : Board -> Coord -> Maybe Cell
cellAt (Board board) ( row, col ) =
    Array.get row board
        `Maybe.andThen` (Array.get col)


countStones : Board -> Stone -> Int
countStones (Board board) player =
    Array.toList board
        |> List.map (Array.filter (\cell -> cell == Occupied player) >> Array.length)
        |> List.sum


emptyMoves : Moves
emptyMoves =
    Moves (Dict.empty)


applyMove : Move -> Board -> Board
applyMove (Move { player, changedCells }) board =
    setStones board player changedCells


moveAt : Moves -> Coord -> Maybe Move
moveAt (Moves dict) coord =
    Dict.get coord dict


validNextMoves : Board -> Stone -> NextMoves
validNextMoves board player =
    let
        currentMoves =
            validMoves board player
    in
        if not (isEmptyMoves currentMoves) then
            ValidMoves player currentMoves
        else
            let
                otherPlayer =
                    otherColor player

                otherMoves =
                    validMoves board otherPlayer
            in
                if not (isEmptyMoves otherMoves) then
                    ValidMoves otherPlayer otherMoves
                else
                    NoValidMoves


validMoves : Board -> Stone -> Moves
validMoves board stone =
    List.concatMap (\r -> List.map (\c -> ( r, c )) [0..7]) [0..7]
        |> List.filterMap
            (\coord ->
                let
                    changed =
                        isValidMove board stone coord
                in
                    if List.isEmpty changed then
                        Nothing
                    else
                        Just
                            ( coord
                            , Move
                                { atCoord = coord
                                , changedCells = changed
                                , player = stone
                                }
                            )
            )
        |> Dict.fromList
        |> Moves


isValidMove : Board -> Stone -> Coord -> List Coord
isValidMove board stone coord =
    case
        List.concatMap
            (isValidMoveInDirection board stone coord)
            [ ( -1, -1 ), ( -1, 0 ), ( -1, 1 ), ( 0, 1 ), ( 1, 1 ), ( 1, 0 ), ( 1, -1 ), ( 0, -1 ) ]
    of
        [] ->
            []

        cells ->
            coord :: cells


isValidMoveInDirection : Board -> Stone -> Coord -> ( Int, Int ) -> List Coord
isValidMoveInDirection board stone coord dir =
    if not (isEmpty board coord) then
        []
    else
        terminatedWith board stone dir (move dir coord) []


terminatedWith : Board -> Stone -> ( Int, Int ) -> Coord -> List Coord -> List Coord
terminatedWith board stone dir coord captured =
    case occupied board coord of
        Just stone' ->
            if stone' == stone then
                captured
            else
                terminatedWith board stone dir (move dir coord) (coord :: captured)

        Nothing ->
            []


setStones : Board -> Stone -> List Coord -> Board
setStones board stone =
    List.foldl (setStone stone) board


setStone : Stone -> Coord -> Board -> Board
setStone stone ( row, col ) (Board board) =
    let
        updateRow oldRow =
            Array.set col (Occupied stone) oldRow
    in
        case Array.get row board of
            Just oldRow ->
                Board (Array.set row (updateRow oldRow) board)

            Nothing ->
                (Board board)


isEmpty : Board -> Coord -> Bool
isEmpty board coord =
    cellAt board coord == Just Empty


occupied : Board -> Coord -> Maybe Stone
occupied board coord =
    cellAt board coord
        `Maybe.andThen`
            (\cell ->
                case cell of
                    Empty ->
                        Nothing

                    Occupied stone ->
                        Just stone
            )


move : ( Int, Int ) -> Coord -> Coord
move ( dx, dy ) ( row, col ) =
    ( row + dx, col + dy )


isEmptyMoves : Moves -> Bool
isEmptyMoves (Moves dict) =
    Dict.isEmpty dict
