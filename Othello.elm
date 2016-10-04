module Othello
    exposing
        ( Board
        , Cell(..)
        , Stone(..)
        , Coord
        , other
        , cellAt
        , setStones
        , isValidMove
        , validMoveCoords
        )

import Array exposing (Array)
import Array as Array


type alias Board =
    Array (Array Cell)


type Cell
    = Empty
    | Occupied Stone


type Stone
    = Black
    | White


type alias Coord =
    { row : Int, col : Int }


other : Stone -> Stone
other stone =
    case stone of
        Black ->
            White

        White ->
            Black


cellAt : Board -> Coord -> Maybe Cell
cellAt board { row, col } =
    Array.get row board
        `Maybe.andThen` (Array.get col)


setStones : Board -> Stone -> List Coord -> Board
setStones board stone =
    List.foldl (setStone stone) board


setStone : Stone -> Coord -> Board -> Board
setStone stone { row, col } board =
    let
        updateRow oldRow =
            Array.set col (Occupied stone) oldRow
    in
        case Array.get row board of
            Just oldRow ->
                Array.set row (updateRow oldRow) board

            Nothing ->
                board


validMoveCoords : Board -> Stone -> List Coord
validMoveCoords board stone =
    List.concatMap (\r -> List.map (\c -> { row = r, col = c }) [0..7]) [0..7]
        |> List.filter (\coord -> not (List.isEmpty (isValidMove board stone coord)))


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
move ( dx, dy ) { row, col } =
    { row = row + dx, col = col + dy }
