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
import Othello exposing (..)


main : Program Never
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
            case isValidMove model.board model.player coord of
                [] ->
                    ( model, Cmd.none )

                _ ->
                    ( { model | highlighted = Just coord }, Cmd.none )

        RemoveHighlight ->
            ( { model | highlighted = Nothing }, Cmd.none )

        ClickAt coord ->
            case isValidMove model.board model.player coord of
                [] ->
                    ( model, Cmd.none )

                coords ->
                    ( { model
                        | board = setStones model.board model.player coords
                        , player = other model.player
                        , highlighted = Nothing
                      }
                    , Cmd.none
                    )


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
                                "#88efa8"
                             else
                                "#77a777"
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
