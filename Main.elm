module Main exposing (..)

import Html exposing (Html)
import Html as Html
import Html.App
import Html.Attributes as Attr
import Html.Events as Events
import Array exposing (Array)
import Array as Array
import Svg exposing (Svg, svg)
import Svg as Svg
import Svg.Attributes as SvgAttr


main =
    Html.App.program
        { init = ( init, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


type alias Model =
    { board : Board
    , player : Stone
    , highlighted : Maybe Coord
    }


type alias Board =
    Array (Array Cell)


type Cell
    = Empty
    | Occupied Stone


type Stone
    = Black
    | White


other : Stone -> Stone
other stone =
    case stone of
        Black ->
            White

        White ->
            Black


type alias Coord =
    { row : Int, col : Int }


isValidMove : Board -> Stone -> Coord -> Bool
isValidMove board stone coord =
    List.any
        (isValidMoveInDirection board stone coord)
        [ ( -1, -1 ), ( -1, 0 ), ( -1, 1 ), ( 0, 1 ), ( 1, 1 ), ( 1, 0 ), ( 1, -1 ), ( 0, -1 ) ]


isValidMoveInDirection : Board -> Stone -> Coord -> ( Int, Int ) -> Bool
isValidMoveInDirection board stone coord dir =
    isEmpty board coord && atLeastOneInDirection board (other stone) dir (move dir coord)


atLeastOneInDirection : Board -> Stone -> ( Int, Int ) -> Coord -> Bool
atLeastOneInDirection board stone dir coord =
    isOccupied board stone coord && terminatedWith board (other stone) dir (move dir coord)


terminatedWith : Board -> Stone -> ( Int, Int ) -> Coord -> Bool
terminatedWith board stone dir coord =
    isOccupied board stone coord
        || let
            next =
                move dir coord
           in
            isOccupied board (other stone) next && terminatedWith board stone dir next


isEmpty : Board -> Coord -> Bool
isEmpty board coord =
    cellAt board coord == Just Empty


isOccupied : Board -> Stone -> Coord -> Bool
isOccupied board stone coord =
    cellAt board coord == Just (Occupied stone)


cellAt : Board -> Coord -> Maybe Cell
cellAt board { row, col } =
    Array.get row board
        `Maybe.andThen` (Array.get col)


move : ( Int, Int ) -> Coord -> Coord
move ( dx, dy ) { row, col } =
    { row = row + dx, col = col + dy }


setStone : Board -> Stone -> Coord -> Board
setStone board stone { row, col } =
    let
        updateRow oldRow =
            Array.set col (Occupied stone) oldRow
    in
        case Array.get row board of
            Just oldRow ->
                Array.set row (updateRow oldRow) board

            Nothing ->
                board


init : Model
init =
    { board =
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
    , player = White
    , highlighted = Nothing
    }


type Message
    = NoOp
    | Highlight Coord
    | RemoveHighlight
    | ClickAt Coord


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Highlight coord ->
            if isValidMove model.board model.player coord then
                ( { model | highlighted = Just coord }, Cmd.none )
            else
                ( model, Cmd.none )

        RemoveHighlight ->
            ( { model | highlighted = Nothing }, Cmd.none )

        ClickAt coord ->
            if isValidMove model.board model.player coord then
                ( { model
                    | board = setStone model.board model.player coord
                    , player = other model.player
                    , highlighted = Nothing
                  }
                , Cmd.none
                )
            else
                ( model, Cmd.none )


view : Model -> Html Message
view model =
    viewBoard model


viewBoard : Model -> Html Message
viewBoard model =
    Html.div
        []
        (List.map (viewRow model) [0..7])


viewRow : Model -> Int -> Html Message
viewRow model row =
    Html.div [ Attr.style [ ( "height", "40px" ) ] ]
        (List.map (viewCell model row) [0..7])


viewCell : Model -> Int -> Int -> Html Message
viewCell model row col =
    cellAt model.board { row = row, col = col }
        |> Maybe.withDefault Empty
        |> let
            coord =
                { row = row, col = col }
           in
            renderCell (model.highlighted == Just coord) coord


renderCell : Bool -> Coord -> Cell -> Html Message
renderCell highlighted coord cell =
    svg
        [ SvgAttr.width "40"
        , SvgAttr.height "40"
        , SvgAttr.viewBox "-5 -5 10 10"
        ]
        (Svg.rect [ SvgAttr.x "-5", SvgAttr.y "-5", SvgAttr.width "10", SvgAttr.height "10", SvgAttr.fill "green" ] []
            :: case cell of
                Empty ->
                    [ Svg.circle
                        [ Attr.style
                            [ ( "cursor"
                              , if highlighted then
                                    "pointer"
                                else
                                    "default"
                              )
                            ]
                        , Events.onMouseEnter (Highlight coord)
                        , Events.onMouseLeave RemoveHighlight
                        , Events.onClick (ClickAt coord)
                        , SvgAttr.r "4.5"
                        , SvgAttr.fill
                            (if highlighted then
                                "#77cf77"
                             else
                                "#88a888"
                            )
                        ]
                        []
                    ]

                Occupied stone ->
                    renderStone stone
        )


renderStone : Stone -> List (Svg Message)
renderStone stone =
    case stone of
        Black ->
            [ Svg.circle [ SvgAttr.r "4.5", SvgAttr.fill "#020202" ] [] ]

        White ->
            [ Svg.circle [ SvgAttr.r "4.5", SvgAttr.fill "#efefef" ] [] ]
