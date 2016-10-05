module MiniMax
    exposing
        ( Value
        , Problem
        , Depth
        , PlayerType(..)
        , miniMax
        , findMax
        )

import Random exposing (minInt, maxInt)


type alias Value =
    Int


type alias Problem state move =
    { heuristic : state -> Value
    , moves : PlayerType -> state -> Maybe ( PlayerType, List move )
    , apply : move -> state -> state
    }


type alias Depth =
    Int


type PlayerType
    = Maximizing
    | Minimizing


findMax : Problem state move -> Depth -> state -> ( Maybe move, Value )
findMax problem depth state =
    miniMax problem depth Maximizing state


miniMax : Problem state move -> Depth -> PlayerType -> state -> ( Maybe move, Value )
miniMax problem depth playerType state =
    let
        bestMove playerType =
            List.map
                (\mv ->
                    let
                        state' =
                            problem.apply mv state

                        ( _, bestVal ) =
                            miniMax problem (depth - 1) (otherType playerType) state'
                    in
                        ( Just mv, bestVal )
                )
                >> searchBest playerType
    in
        if depth == 0 then
            ( Nothing, problem.heuristic state )
        else
            case problem.moves playerType state of
                Just ( playerType, moves ) ->
                    bestMove playerType moves

                Nothing ->
                    ( Nothing, problem.heuristic state )


searchBest : PlayerType -> List ( Maybe move, Value ) -> ( Maybe move, Value )
searchBest playerType =
    let
        cmp =
            case playerType of
                Maximizing ->
                    (>)

                Minimizing ->
                    (<)

        compare =
            (\( mv, val ) ( bestMv, bestVal ) ->
                if val `cmp` bestVal then
                    ( mv, val )
                else
                    ( bestMv, bestVal )
            )

        start =
            case playerType of
                Maximizing ->
                    ( Nothing, minInt )

                Minimizing ->
                    ( Nothing, maxInt )
    in
        List.foldl compare start


otherType : PlayerType -> PlayerType
otherType playerType =
    case playerType of
        Minimizing ->
            Maximizing

        Maximizing ->
            Minimizing
