module Types exposing (..)

import Array exposing (..)
import Time exposing (Time)
import Mouse exposing (Position)


type alias IndexedGrid =
    Array (Array Bool)


type alias Grid =
    List (List Bool)


type alias Model =
    { grid : Grid
    , text : String
    , timeIsTicking : Bool
    , speed : Float
    }


type Msg
    = Tick Time
    | Stop
    | Start
    | AdjustSpeed Float
    | MouseClick Position


type alias Cell =
    { x : Int, y : Int }


indexGrid : Grid -> IndexedGrid
indexGrid grid =
    grid |> fromList |> Array.map fromList


indexGridToGrid : IndexedGrid -> Grid
indexGridToGrid indexedGrid =
    indexedGrid |> toList |> List.map toList


gridToIndexedList : Grid -> List ( Int, List ( Int, Bool ) )
gridToIndexedList grid =
    grid |> (List.indexedMap (,)) |> List.map (\( i, b ) -> ( i, (List.indexedMap (,) b) ))
