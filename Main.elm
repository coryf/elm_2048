module Main exposing (main)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Keyboard
import Random
import Dict exposing (Dict)
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


type alias Cells =
    Dict Point Int


type alias Point =
    ( Int, Int )


type alias Board =
    { size : Int
    , cells : Cells
    }


type alias Model =
    { board : Board
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
    }


initModel : Model
initModel =
    { board = initBoard 4
    , randomSeed = Random.initialSeed 0
    }
        |> insertRandomCell
        |> insertRandomCell


listGet : Int -> List a -> Maybe a
listGet n xs =
    List.head (List.drop n xs)


generate2or4 : Random.Generator Int
generate2or4 =
    Random.int 1 2 |> Random.map ((*) 2)


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


slideCell : Cells -> SlideDirection -> Point -> Int -> Int
slideCell cells direction ( x, y ) num =
    if num == 0 then
        case Dict.get ( x + 1, y ) cells of
            Just n ->
                n

            Nothing ->
                0
    else
        num


listChunk2 : a -> List a -> List ( a, a )
listChunk2 default list =
    case list of
        [] ->
            []

        [ x ] ->
            [ ( x, default ) ]

        x1 :: x2 :: xs ->
            ( x1, x2 ) :: (listChunk2 default xs)


slideCellSlice : Board -> SlideDirection -> Int -> List ( Point, Int )
slideCellSlice { cells, size } direction sliceIndex =
    let
        ( swapDimensions, sliceDimension, swapSlice ) =
            case direction of
                SlideLeft ->
                    ( \( x, y ) -> ( y, x ), Tuple.second, identity )

                SlideRight ->
                    ( \( x, y ) -> ( y, x ), Tuple.second, List.reverse )

                SlideUp ->
                    ( identity, Tuple.first, identity )

                SlideDown ->
                    ( identity, Tuple.first, List.reverse )

        boardSlice : Cells -> List Int
        boardSlice cells =
            cells
                |> Dict.filter (\p _ -> sliceDimension p == sliceIndex)
                |> Dict.values
                |> swapSlice

        cellSliceCompact : List Int -> List Int
        cellSliceCompact =
            List.filter (\n -> n /= 0)

        cellSliceExpand : List Int -> List ( Point, Int )
        cellSliceExpand slice =
            let
                compactSize =
                    List.length slice

                filledSlice =
                    slice ++ List.repeat (size - compactSize) 0
            in
                filledSlice
                    |> swapSlice
                    |> List.indexedMap
                        (\i n ->
                            ( swapDimensions ( sliceIndex, i ), n )
                        )

        cellSliceAdd : List Int -> List Int
        cellSliceAdd slice =
            slice
                |> List.foldl
                    (\n ( acc, hop ) ->
                        case acc of
                            [] ->
                                ( [ n ], False )

                            x :: xs ->
                                if hop || n /= x then
                                    ( n :: x :: xs, False )
                                else
                                    ( (n + n) :: xs, True )
                    )
                    ( [], False )
                |> Tuple.first
                |> List.reverse
    in
        cells
            |> boardSlice
            |> cellSliceCompact
            |> cellSliceAdd
            |> cellSliceExpand


slideCells : Model -> SlideDirection -> Model
slideCells ({ board } as model) direction =
    let
        cells =
            List.range 0 (board.size - 1)
                |> List.map (slideCellSlice board direction)
                |> List.concat
                |> Dict.fromList
    in
        if cells == board.cells then
            model
        else
            { model | board = { board | cells = cells } }
                |> insertRandomCell


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
            slideCells model SlideLeft

        'A' ->
            slideCells model SlideLeft

        'L' ->
            slideCells model SlideRight

        'D' ->
            slideCells model SlideRight

        'K' ->
            slideCells model SlideUp

        'W' ->
            slideCells model SlideUp

        'J' ->
            slideCells model SlideDown

        'S' ->
            slideCells model SlideDown

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


cellView : Int -> ( Point, Int ) -> List (Svg Msg)
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
            , fill "#eee"
            , stroke "#555"
            , strokeWidth "2"
            , rx "8"
            , ry "8"
            ]
            []
        ]
            ++ cellText


backgroundView : Svg Msg
backgroundView =
    rect [ fill "#555", x "0", y "0", width "600", height "600" ] []


boardView : Board -> Svg Msg
boardView { size, cells } =
    let
        squareSize =
            600 // size

        cellElems =
            cells |> Dict.toList |> List.map (cellView squareSize) |> List.concat
    in
        g [] cellElems


view : Model -> Html Msg
view model =
    svg [ width "100%", height "100%", viewBox "0 0 600 600" ]
        [ backgroundView
        , boardView model.board
        ]
