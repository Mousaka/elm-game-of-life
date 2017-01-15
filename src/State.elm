module State exposing (update, model)

import Types exposing (..)
import Array exposing (..)


model : Model
model =
    { grid = initGrid
    , text = "hej"
    }


initGrid : List (List Bool)
initGrid =
    [ [ False, True, False ] ++ (padDeadCells 97)
    , [ False, False, True ] ++ (padDeadCells 97)
    , [ True, True, True ] ++ (padDeadCells 97)
    ]
        ++ (padDeadRow 97)


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick ->
            lifeGoesOn model


padDeadRow : Int -> List (List Bool)
padDeadRow n =
    List.range 1 n |> List.map (\_ -> padDeadCells 100)


padDeadCells : Int -> List Bool
padDeadCells n =
    List.range 1 n |> List.map (\_ -> False)


lifeGoesOn : Model -> Model
lifeGoesOn model =
    { model | text = model.text ++ "!", grid = killAndSpawn model.grid }


indexedListToGrid : List ( Int, List ( Int, Bool ) ) -> Grid
indexedListToGrid indexedList =
    let
        removeIndex =
            (\( _, b ) -> b)
    in
        indexedList |> List.map removeIndex |> List.map (\x -> List.map removeIndex x)


killAndSpawn : Grid -> Grid
killAndSpawn grid =
    let
        indexedGrid =
            indexGrid grid
    in
        indexedListToGrid <| List.map (killAndSpawnRow indexedGrid) (toIndexedList indexedGrid)


killAndSpawnRow : IndexedGrid -> ( Int, Array Bool ) -> ( Int, List ( Int, Bool ) )
killAndSpawnRow grid ( y, row ) =
    ( y, deathRow grid y (toIndexedList row) )


deathRow : IndexedGrid -> Int -> List ( Int, Bool ) -> List ( Int, Bool )
deathRow grid y row =
    case row of
        ( x_, alive ) :: t ->
            let
                neighbours =
                    countLivingNeighbours grid x_ y
            in
                if alive then
                    if neighbours > 3 || neighbours < 2 then
                        [ ( x_, False ) ] ++ deathRow grid y t
                    else
                        [ ( x_, True ) ] ++ deathRow grid y t
                else if neighbours == 3 then
                    [ ( x_, True ) ] ++ deathRow grid y t
                else
                    [ ( x_, False ) ] ++ deathRow grid y t

        [] ->
            []


countLivingNeighbours : IndexedGrid -> Int -> Int -> Int
countLivingNeighbours grid x y =
    let
        getNeighbourCoords x y =
            [ ( x - 1, y + 1 )
            , ( x, y + 1 )
            , ( x + 1, y + 1 )
            , ( x - 1, y )
            , ( x + 1, y )
            , ( x - 1, y - 1 )
            , ( x, y - 1 )
            , ( x + 1, y - 1 )
            ]

        ncoords =
            getNeighbourCoords x y
    in
        List.length <| List.filter (gridGetter grid) ncoords


gridGetter : IndexedGrid -> ( Int, Int ) -> Bool
gridGetter grid ( x, y ) =
    let
        row =
            Maybe.withDefault (fromList []) <| get y grid
    in
        Maybe.withDefault False <| get x row
