module Types exposing (..)

import Array exposing (..)


type alias IndexedGrid =
    Array (Array Bool)


type alias Grid =
    List (List Bool)


type alias Model =
    { grid : Grid
    , text : String
    }


type Msg
    = Tick


indexGrid : Grid -> IndexedGrid
indexGrid grid =
    grid |> fromList |> Array.map fromList


indexGridToGrid : IndexedGrid -> Grid
indexGridToGrid indexedGrid =
    indexedGrid |> toList |> List.map toList


gridToIndexedList : Grid -> List ( Int, List ( Int, Bool ) )
gridToIndexedList grid =
    grid |> (List.indexedMap (,)) |> List.map (\( i, b ) -> ( i, (List.indexedMap (,) b) ))
