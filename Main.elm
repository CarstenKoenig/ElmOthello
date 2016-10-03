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
    , highlighted : Maybe Coord
    }


type alias Board =
    Array (Array Cell)


type alias Cell =
    Maybe Stone


type Stone
    = Black
    | White


type alias Coord =
    { row : Int, col : Int }


init : Model
init =
    { board =
        [ List.repeat 8 Nothing
        , List.repeat 8 Nothing
        , List.repeat 8 Nothing
        , [ Nothing, Nothing, Nothing, Just Black, Just White, Nothing, Nothing, Nothing ]
        , [ Nothing, Nothing, Nothing, Just White, Just Black, Nothing, Nothing, Nothing ]
        , List.repeat 8 Nothing
        , List.repeat 8 Nothing
        , List.repeat 8 Nothing
        ]
            |> List.map Array.fromList
            |> Array.fromList
    , highlighted = Nothing
    }


type Message
    = NoOp
    | Highlight Coord


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Highlight coord ->
            ( { model | highlighted = Just coord }, Cmd.none )


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
    Array.get row model.board
        `Maybe.andThen` (Array.get col)
        |> Maybe.withDefault Nothing
        |> let
            coord =
                { row = row, col = col }
           in
            renderCell (model.highlighted == Just coord) coord


renderCell : Bool -> Coord -> Cell -> Html Message
renderCell highlighted coord cell =
    svg
        [ Events.onMouseOver (Highlight coord), SvgAttr.width "40", SvgAttr.height "40", SvgAttr.viewBox "-5 -5 10 10" ]
        (Svg.rect [ SvgAttr.x "-5", SvgAttr.y "-5", SvgAttr.width "10", SvgAttr.height "10", SvgAttr.fill "green" ] []
            :: case cell of
                Nothing ->
                    [ Svg.circle
                        [ Attr.style [ ( "cursor", "pointer" ) ]
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

                Just stone ->
                    renderStone stone
        )


renderStone : Stone -> List (Svg Message)
renderStone stone =
    case stone of
        Black ->
            [ Svg.circle [ SvgAttr.r "4.5", SvgAttr.fill "#020202" ] [] ]

        White ->
            [ Svg.circle [ SvgAttr.r "4.5", SvgAttr.fill "#efefef" ] [] ]
