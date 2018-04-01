module Main exposing (..)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Keyboard
import Random
import Dict exposing (Dict)
import Array exposing (Array)
import Char
import Debug


-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Cell =
    ( Point, Int )


type alias Cells =
    Dict Point Int


type alias Point =
    ( Int, Int )


type alias Board =
    { size : Int
    , cells : Cells
    , slideAnimations : List ( Point, Point, Int )
    , mergeAnimations : List ( Point, Point, Int )
    }


type alias Model =
    { board : Board
    , score : Int
    , gameOver : Bool
    , randomSeed : Random.Seed
    }


initCells : Int -> Cells
initCells size =
    let
        last =
            size - 1
    in
        List.range 0 last
            |> List.concatMap
                (\x ->
                    List.range 0 last
                        |> List.concatMap (\y -> [ ( ( x, y ), 0 ) ])
                )
            |> Dict.fromList


initBoard : Int -> Board
initBoard size =
    { size = size
    , cells = initCells size
    , slideAnimations = []
    , mergeAnimations = []
    }


initModel : Model
initModel =
    { board = initBoard 3
    , score = 0
    , gameOver = False
    , randomSeed = Random.initialSeed 0
    }
        |> insertRandomCell
        |> insertRandomCell


listGet : Int -> List a -> Maybe a
listGet n xs =
    List.head (List.drop n xs)


generate2or4 : Random.Generator Int
generate2or4 =
    Random.float 0 1
        |> Random.map
            (\p ->
                if p < 0.9 then
                    2
                else
                    4
            )


openCellList : Cells -> List Point
openCellList cells =
    cells
        |> Dict.filter (\_ n -> n == 0)
        |> Dict.keys


generateOpenPoint : Cells -> Random.Generator (Maybe Point)
generateOpenPoint cells =
    let
        openCells =
            openCellList cells

        last =
            (List.length openCells) - 1
    in
        Random.int 0 last |> Random.map (\n -> listGet n openCells)


generateNewCell : Cells -> Random.Generator ( Maybe Point, Int )
generateNewCell cells =
    Random.pair (generateOpenPoint cells) generate2or4


insertRandomCell : Model -> Model
insertRandomCell ({ board, randomSeed } as model) =
    let
        ( ( maybePoint, num ), randomSeed_ ) =
            Random.step (generateNewCell board.cells) randomSeed

        board_ =
            case maybePoint of
                Nothing ->
                    board

                Just point ->
                    { board | cells = Dict.insert point num board.cells }
    in
        { model | board = board_, randomSeed = randomSeed_ }


type SlideDirection
    = SlideLeft
    | SlideRight
    | SlideUp
    | SlideDown


tuple2Swap : ( a, b ) -> ( b, a )
tuple2Swap ( a, b ) =
    ( b, a )


slideCellSlice : Board -> SlideDirection -> Int -> List Cell
slideCellSlice { cells, size } direction sliceIndex =
    let
        ( swapDimensions, sliceDimension, swapSlice ) =
            case direction of
                SlideLeft ->
                    ( tuple2Swap, Tuple.second, identity )

                SlideRight ->
                    ( tuple2Swap, Tuple.second, List.reverse )

                SlideUp ->
                    ( identity, Tuple.first, identity )

                SlideDown ->
                    ( identity, Tuple.first, List.reverse )

        boardSlice : Cells -> List Cell
        boardSlice cells =
            cells
                |> Dict.filter (\p _ -> sliceDimension p == sliceIndex)
                |> Dict.toList
                |> swapSlice

        cellSliceCompact : List Cell -> List Cell
        cellSliceCompact =
            List.filter (\( _, n ) -> n /= 0)

        cellSliceMerge : List Cell -> List Cell
        cellSliceMerge slice =
            slice
                |> List.foldl
                    (\( p, n ) ( acc, hop ) ->
                        case acc of
                            [] ->
                                ( [ ( p, n ) ], False )

                            ( pHead, nHead ) :: xs ->
                                if hop || n /= nHead then
                                    ( ( p, n ) :: ( pHead, nHead ) :: xs, False )
                                else
                                    ( ( p, n + n ) :: xs, True )
                    )
                    ( [], False )
                |> Tuple.first
                |> List.reverse

        cellSliceExpand : List Cell -> List Cell
        cellSliceExpand slice =
            let
                compactSize =
                    List.length slice

                valueSlice =
                    slice |> List.map Tuple.second

                filledSlice =
                    valueSlice ++ List.repeat (size - compactSize) 0
            in
                filledSlice
                    |> swapSlice
                    |> List.indexedMap
                        (\i n ->
                            ( swapDimensions ( sliceIndex, i ), n )
                        )
    in
        cells
            |> boardSlice
            |> cellSliceCompact
            |> cellSliceMerge
            |> cellSliceExpand


slideCells : Board -> SlideDirection -> Board
slideCells board direction =
    { board
        | cells =
            List.range 0 (board.size - 1)
                |> List.map (slideCellSlice board direction)
                |> List.concat
                |> Dict.fromList
    }


canSlide : Board -> Bool
canSlide board =
    [ SlideLeft, SlideRight, SlideUp, SlideDown ]
        |> List.map (slideCells board)
        |> List.any (\board_ -> board_.cells /= board.cells)


handleSlide : Model -> SlideDirection -> Model
handleSlide ({ board } as model) direction =
    let
        board_ =
            slideCells board direction

        slid =
            board_.cells /= board.cells

        model_ =
            if slid then
                { model | board = board_ } |> insertRandomCell
            else
                { model | board = board_ }
    in
        { model_ | gameOver = not (canSlide model_.board) }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- UPDATE


type Msg
    = KeyDownMsg Keyboard.KeyCode


handleKeyDown : Model -> Keyboard.KeyCode -> Model
handleKeyDown model keyCode =
    case Char.fromCode keyCode of
        'H' ->
            handleSlide model SlideLeft

        'A' ->
            handleSlide model SlideLeft

        'L' ->
            handleSlide model SlideRight

        'D' ->
            handleSlide model SlideRight

        'K' ->
            handleSlide model SlideUp

        'W' ->
            handleSlide model SlideUp

        'J' ->
            handleSlide model SlideDown

        'S' ->
            handleSlide model SlideDown

        'R' ->
            initModel

        _ ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDownMsg keyCode ->
            ( handleKeyDown model keyCode, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDownMsg
        ]



-- VIEW


cellDefaultColor : String
cellDefaultColor =
    "#eee"


cellColors : Array String
cellColors =
    Array.fromList
        [ cellDefaultColor -- 0
        , "#eee4da" -- 2
        , "#ede0c8" -- 4
        , "#f2b179" -- 8
        , "#f59563" -- 16
        , "#f67c5f" -- 32
        , "#f65e3b" -- 64
        , "#edcf72" -- 128
        , "#edcc61" -- 256
        , "#edc850" -- 512
        , "#edc53f" -- 1024
        , "#edc22e" -- 2048
        ]


getCellColor : Int -> String
getCellColor num =
    let
        index =
            num |> toFloat |> logBase 2 |> truncate
    in
        Array.get index cellColors |> Maybe.withDefault cellDefaultColor


cellView : Int -> Cell -> List (Svg Msg)
cellView size ( ( pX, pY ), num ) =
    let
        ( x_, y_ ) =
            ( (pX * size) + 1, (pY * size) + 1 )

        size_ =
            size - 2

        ( cX, cY ) =
            ( x_ + size_ // 2, y_ + size_ // 2 )

        cellText =
            if num /= 0 then
                [ text_
                    [ x (toString cX)
                    , y (toString cY)
                    , textAnchor "middle"
                    , alignmentBaseline "middle"
                    , fontSize "32"
                    ]
                    [ text (toString num) ]
                ]
            else
                []
    in
        [ rect
            [ x (toString x_)
            , y (toString y_)
            , width (toString size_)
            , height (toString size_)
            , fill (num |> getCellColor)
            , stroke "#555"
            , strokeWidth "2"
            , rx "8"
            , ry "8"
            ]
            []
        ]
            ++ cellText


backgroundView : List (Svg Msg)
backgroundView =
    [ rect [ fill "#555", x "0", y "0", width "600", height "800" ] [] ]


headerView : Model -> List (Svg Msg)
headerView model =
    [ rect
        [ fill "#ddd"
        , stroke "#555"
        , x "2"
        , y "2"
        , width "596"
        , height "196"
        , strokeWidth "2"
        , rx "8"
        , ry "8"
        ]
        []
    , text_
        [ x "100"
        , y "100"
        , fontSize "48"
        , textAnchor "left"
        , alignmentBaseline "middle"
        ]
        [ text (model.score |> toString) ]
    ]


boardView : Board -> List (Svg Msg)
boardView { size, cells } =
    let
        squareSize =
            600 // size

        cellElems =
            cells |> Dict.toList |> List.map (cellView squareSize) |> List.concat
    in
        [ g [ transform "translate(0 200)" ] cellElems ]


gameOverView : Bool -> List (Svg Msg)
gameOverView gameOver =
    if gameOver then
        [ rect [ fill "red", x "0", y "0", opacity "0.20", width "600", height "800" ] []
        ]
    else
        []


view : Model -> Html Msg
view model =
    svg [ width "100%", height "100%", viewBox "0 0 600 800" ]
        (backgroundView
            ++ headerView model
            ++ boardView model.board
            ++ gameOverView model.gameOver
        )
