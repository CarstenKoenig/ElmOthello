module Main exposing (..)

import AI exposing (..)
import Html as Html
import Html exposing (Html)
import Html.App
import Html.Attributes as Attr
import Html.Events as Events
import Othello exposing (..)
import Svg as Svg
import Svg exposing (Svg, svg)
import Svg.Attributes as SvgAttr
import Task exposing (Task, fail, succeed)


main : Program Never
main =
    Html.App.program
        { init = ( init, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


type GameState
    = Moving Stone
    | GameOver


type alias Model =
    { board : Board
    , game : GameState
    , hoover : Maybe Coord
    , validMoves : Moves
    }


init : Model
init =
    { board = startBoard
    , game = Moving White
    , hoover = Nothing
    , validMoves = initialValidMoves
    }


currentPlayer : Model -> Maybe Stone
currentPlayer model =
    case model.game of
        GameOver ->
            Nothing

        Moving player ->
            Just player


type Message
    = NoOp
    | Hoover (Maybe Coord)
    | ClickAt Coord
    | BlackAiMoved Move
    | BlackAiFailed


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Hoover over ->
            case over of
                Just coord ->
                    if isValidMove model coord then
                        ( { model | hoover = Just coord }, Cmd.none )
                    else
                        ( { model | hoover = Nothing }, Cmd.none )

                Nothing ->
                    ( { model | hoover = Nothing }, Cmd.none )

        BlackAiMoved move ->
            computerMoves model move

        BlackAiFailed ->
            ( { model
                | game = GameOver
                , hoover = Nothing
                , validMoves = emptyMoves
              }
            , Cmd.none
            )

        ClickAt coord ->
            case model.game of
                GameOver ->
                    ( model, Cmd.none )

                Moving Black ->
                    ( model, Cmd.none )

                Moving White ->
                    humanMoves model coord


isValidMove : Model -> Coord -> Bool
isValidMove model coord =
    case moveAtCoord model.validMoves coord of
        Just _ ->
            True

        Nothing ->
            False


calculateAiMove : Board -> Cmd Message
calculateAiMove board =
    Task.perform (\_ -> BlackAiFailed) BlackAiMoved (computerMove board)


computerMove : Board -> Task () Move
computerMove board =
    case blackAI 3 board of
        Nothing ->
            fail ()

        Just move ->
            succeed move


computerMoves : Model -> Move -> ( Model, Cmd Message )
computerMoves model move =
    let
        board' =
            applyMove move model.board

        nextMoves =
            validNextMoves board' White
    in
        case nextMoves of
            NoValidMoves ->
                ( { model
                    | board = board'
                    , game = GameOver
                    , hoover = Nothing
                    , validMoves = emptyMoves
                  }
                , Cmd.none
                )

            ValidMoves White moves ->
                ( { model
                    | board = board'
                    , game = Moving White
                    , hoover = Nothing
                    , validMoves = moves
                  }
                , Cmd.none
                )

            ValidMoves Black moves ->
                ( { model
                    | board = board'
                    , game = Moving Black
                    , hoover = Nothing
                    , validMoves = emptyMoves
                  }
                , calculateAiMove board'
                )


humanMoves : Model -> Coord -> ( Model, Cmd Message )
humanMoves model coord =
    case moveAtCoord model.validMoves coord of
        Nothing ->
            ( model, Cmd.none )

        Just move ->
            let
                board' =
                    applyMove move model.board
            in
                ( { model
                    | board = board'
                    , game = Moving Black
                    , hoover = Nothing
                    , validMoves = emptyMoves
                  }
                , calculateAiMove board'
                )


view : Model -> Html Message
view model =
    Html.div
        []
        [ Html.h1 [] [ Html.text (points model ++ " = " ++ statusText model) ]
        , viewBoard model
        ]


statusText : Model -> String
statusText model =
    case model.game of
        GameOver ->
            "Game Over - " ++ winner model

        Moving Black ->
            "Black's turn"

        Moving White ->
            "White's turn"


winner : Model -> String
winner model =
    let
        whites =
            countStones model.board White

        blacks =
            countStones model.board Black
    in
        if whites == blacks then
            "draw"
        else if whites < blacks then
            "Black WON"
        else
            "White WON"


points : Model -> String
points model =
    let
        whites =
            countStones model.board White

        blacks =
            countStones model.board Black
    in
        toString whites ++ ":" ++ toString blacks


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
    cellAt model.board ( row, col )
        |> Maybe.withDefault Empty
        |> let
            coord =
                ( row, col )
           in
            renderCell
                (isValidMove model coord)
                (model.hoover == Just coord)
                (currentPlayer model)
                coord


renderCell : Bool -> Bool -> Maybe Stone -> Coord -> Cell -> Html Message
renderCell highlighted hooverOver player coord cell =
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
                        , Events.onMouseEnter (Hoover (Just coord))
                        , Events.onMouseLeave (Hoover Nothing)
                        , Events.onClick (ClickAt coord)
                        , SvgAttr.r "4.5"
                        , SvgAttr.fill
                            (if hooverOver then
                                stoneColor player
                             else if highlighted then
                                highlightColor
                             else
                                holeColor
                            )
                        ]
                        []
                    ]

                Occupied stone ->
                    renderStone (Just stone)
        )


renderStone : Maybe Stone -> List (Svg Message)
renderStone stone =
    [ Svg.circle [ SvgAttr.r "4.5", SvgAttr.fill (stoneColor stone) ] [] ]


stoneColor : Maybe Stone -> String
stoneColor stone =
    case stone of
        Just White ->
            whiteColor

        Just Black ->
            blackColor

        Nothing ->
            holeColor


whiteColor : String
whiteColor =
    "#efefef"


blackColor : String
blackColor =
    "#020202"


highlightColor : String
highlightColor =
    "#77c777"


holeColor : String
holeColor =
    "#77a777"
