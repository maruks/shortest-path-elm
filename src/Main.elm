module Main exposing (..)

import Debug exposing (log)
import Dict exposing (Dict, empty, fromList, toList)
import Html exposing (Html)
import List exposing (..)
import Random
import String exposing (join)
import Svg exposing (Svg, line, rect, svg)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Task
import Window


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, Task.perform SetScreenSize Window.size )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Point =
    ( Int, Int )


type alias Path =
    List Point


type alias RandCells =
    List Int


type alias CellsDict =
    Dict Point Int


type alias Grid =
    { width : Int, height : Int, cellSize : Int }


type alias Model =
    { grid : Grid, cells : CellsDict, selected : Maybe Point, path : Path }


initModel : Model
initModel =
    { grid = { width = 0, height = 0, cellSize = 1 }
    , cells = empty
    , selected = Nothing
    , path = []
    }



-- shortest path


type alias Cost =
    { cost : Int, via : Point }


type alias Nodes =
    Dict Point Cost


type alias CostFn =
    Point -> Point -> Int


type alias ConnectedPointsFn =
    Point -> List Point


getPath : Nodes -> Point -> Path
getPath nodes point =
    case Dict.get point nodes of
        Nothing ->
            []

        Just { cost, via } ->
            if via == point then
                List.singleton point
            else
                point :: getPath nodes via


minNode : List ( Point, Cost ) -> Maybe ( Point, Cost )
minNode nodes =
    case nodes |> head of
        Nothing ->
            Nothing

        Just first ->
            Just <|
                foldr
                    (\( point, cost ) ( minPoint, minCost ) ->
                        if cost.cost < minCost.cost then
                            ( point, cost )
                        else
                            ( minPoint, minCost )
                    )
                    first
                    nodes


findPath : Maybe ( Point, Cost ) -> Nodes -> Nodes -> Point -> CostFn -> ConnectedPointsFn -> Path
findPath minPoint nodes visited end costFn connFn =
    case minPoint of
        Nothing ->
            []

        Just ( point, minCost ) ->
            if point == end then
                getPath (Dict.union nodes visited) point
            else
                let
                    pts =
                        connFn point |> List.filter (\p -> not <| Dict.member p visited)

                    { cost, via } =
                        minCost

                    newVisited =
                        Dict.insert point minCost visited

                    costs =
                        map (\p -> ( p, costFn point p )) pts

                    newNodes =
                        foldr
                            (\( p, c ) n ->
                                Dict.update p
                                    (\v ->
                                        case v of
                                            Nothing ->
                                                Just { cost = cost + c, via = point }

                                            Just j ->
                                                if cost + c < j.cost then
                                                    Just { cost = cost + c, via = point }
                                                else
                                                    v
                                    )
                                    n
                            )
                            (Dict.remove point nodes)
                            costs

                    newMinPoint =
                        minNode <| Dict.toList newNodes
                in
                findPath newMinPoint newNodes newVisited end costFn connFn


getCost : CellsDict -> Point -> Point -> Point -> Int
getCost cells ( ex, ey ) p1 ( x, y ) =
    let
        height =
            case Dict.get ( x, y ) cells of
                Nothing ->
                    0

                Just h ->
                    h

        dx =
            ex - x

        dy =
            ey - y

        d =
            dx * dx + dy * dy |> toFloat |> sqrt |> round
    in
    1 + height // 10


connectedPoints : CellsDict -> Point -> List Point
connectedPoints cells ( x, y ) =
    let
        ns =
            [ ( 1, 0 ), ( 0, 1 ), ( -1, 0 ), ( 0, -1 ) ]

        pts =
            map (\( dx, dy ) -> ( x + dx, y + dy )) ns
    in
    List.filter (\p -> Dict.member p cells) pts


shortestPath : CellsDict -> Point -> Point -> Path
shortestPath cells start end =
    let
        nodes =
            Dict.singleton start { cost = 0, via = start }

        queue =
            Dict.toList nodes |> head

        costFn =
            getCost cells end

        connFn =
            connectedPoints cells
    in
    findPath queue nodes Dict.empty end costFn connFn



-- UPDATE


type Msg
    = SetScreenSize Window.Size
    | InitGrid RandCells
    | SelectCell Point


initCells : RandCells -> Int -> Int -> CellsDict
initCells hs width height =
    let
        xs =
            range 0 (width - 1)

        ys =
            range 0 (height - 1)

        points =
            concatMap (\x -> map (\y -> ( x, y )) ys) xs

        kvs =
            map2 (,) points <| map (\h -> if h<180 then 0 else h) hs
    in
    fromList kvs


blur : CellsDict -> ( Int, Int ) -> Int
blur cells ( x, y ) =
    let
        xs =
            range (x - 1) (x + 1)

        ys =
            range (y - 1) (y + 1)

        points =
            concatMap (\x -> map (\y -> ( x, y )) ys) xs

        ( sum, num ) =
            foldr
                (\p ( s, n ) ->
                    case Dict.get p cells of
                        Just h ->
                            ( s + h, n + 1 )

                        Nothing ->
                            ( s, n )
                )
                ( 0, 0 )
                points
    in
    sum // num


blurCells : CellsDict -> CellsDict
blurCells cells =
    Dict.map (\point _ -> blur cells point) cells


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { grid, cells } =
            model
    in
    case msg of
        SetScreenSize { width, height } ->
            let
                c =
                    Basics.max 10 (width // 90)

                w =
                    width // c - 1

                h =
                    height // c - 1

                newGrid =
                    { width = w, height = h, cellSize = c }
            in
            if grid == newGrid then
                ( model, Cmd.none )
            else
                ( { model | grid = newGrid, path = [] }, Random.generate InitGrid (Random.int 0 255 |> Random.list (w * h)) )

        InitGrid randGrid ->
            ( { model | cells = initCells randGrid grid.width grid.height |> blurCells }, Cmd.none )

        SelectCell point ->
            case model.selected of
                Nothing ->
                    ( { model | selected = Just point }, Cmd.none )

                Just start ->
                    let
                        shortest =
                            shortestPath cells start point
                    in
                    ( { model | selected = Nothing, path = shortest }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Window.resizes SetScreenSize



-- VIEW


renderGrid : Int -> Int -> Int -> List (Svg msg)
renderGrid width height cellSize =
    let
        xs =
            range 0 width |> map (\x -> x * cellSize)

        ys =
            range 0 height |> map (\x -> x * cellSize)

        ws =
            width * cellSize |> toString

        hs =
            height * cellSize |> toString

        lineStyle =
            "stroke:#999999;stroke-width:1"

        xls =
            List.map (\x -> line [ x1 (toString x), y1 "0", x2 (toString x), y2 hs, style lineStyle ] []) xs

        yls =
            List.map (\y -> line [ x1 "0", y1 (toString y), x2 ws, y2 (toString y), style lineStyle ] []) ys
    in
    xls ++ yls


fillColor : Int -> String
fillColor h =
    "fill:rgb(255," ++ toString (229 - round (0.3764 * toFloat h)) ++ "," ++ toString (200 - round (0.7843 * toFloat h)) ++ ")"


renderCells : Int -> CellsDict -> List (Svg Msg)
renderCells size =
    toList >> List.map (\( ( x, y ), h ) -> rect [ SelectCell ( x, y ) |> onClick, Svg.Attributes.x (toString (x * size + 1)), Svg.Attributes.y (toString (y * size + 1)), Svg.Attributes.width (toString (size - 1)), Svg.Attributes.height (toString (size - 1)), style (fillColor h) ] [])


renderPath : Int -> Path -> List (Svg Msg)
renderPath cellSize path =
    if isEmpty path then
        []
    else
        let
            pointsStr =
                map (\( x, y ) -> toString (x * cellSize + cellSize // 2) ++ "," ++ toString (y * cellSize + cellSize // 2)) path |> join " "

            width = Basics.max 1 <| cellSize // 7

            lines =
                Svg.polyline [ Svg.Attributes.style <| "fill:none;stroke:black;stroke-width:" ++ (toString width) , Svg.Attributes.points pointsStr ] []
        in
        singleton lines


view : Model -> Html Msg
view { grid, cells, path } =
    let
        { width, height, cellSize } =
            grid

        g =
            renderGrid width height cellSize

        c =
            renderCells cellSize cells

        p =
            renderPath cellSize path
    in
    svg
        [ Svg.Attributes.width (width * cellSize |> toString), Svg.Attributes.height (height * cellSize |> toString) ]
        (c ++ g ++ p)
