module MiniMax
    exposing
        ( Value
        , Heuristic
        , ChildGen
        , Depth
        , PlayerType
        , miniMax
        , findMax
        )


type alias Value =
    Int


type alias Heuristic state =
    state -> Value


type alias ChildGen state =
    state -> List state


type alias Depth =
    Int


type PlayerType
    = Maximizing
    | Minimizing


findMax : Heuristic state -> ChildGen state -> Depth -> state -> Value
findMax heuristic children depth state =
    miniMax heuristic children depth Maximizing state


miniMax : Heuristic state -> ChildGen state -> Depth -> PlayerType -> state -> Value
miniMax heuristic children depth playerType state =
    let
        bestOfChildren cs =
            List.map (miniMax heuristic children (depth - 1) (otherType playerType)) cs
                |> accumChildHeuristics playerType
    in
        if depth == 0 then
            heuristic state
        else
            case bestOfChildren (children state) of
                Nothing ->
                    heuristic state

                Just value ->
                    value


otherType : PlayerType -> PlayerType
otherType playerType =
    case playerType of
        Maximizing ->
            Minimizing

        Minimizing ->
            Maximizing


accumChildHeuristics : PlayerType -> (List Int -> Maybe Int)
accumChildHeuristics playerType =
    case playerType of
        Maximizing ->
            List.maximum

        Minimizing ->
            List.minimum
