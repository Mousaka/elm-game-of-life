module State exposing (update, init)

import Types exposing (..)
import Array exposing (..)
import Mouse exposing (Position)


init : ( Model, Cmd Msg )
init =
    ( { grid = initGrid
      , text = ""
      , timeIsTicking = True
      , speed = 1
      }
    , Cmd.none
    )


initGrid : List (List Bool)
initGrid =
    [ [ False, False, False ] ++ (padDeadCells 97)
    , [ False, False, False ] ++ (padDeadCells 97)
    , [ False, False, False ] ++ (padDeadCells 97)
    ]
        ++ (padDeadRow 97)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateUtil msg model, Cmd.none )


updateUtil : Msg -> Model -> Model
updateUtil msg model =
    case msg of
        Tick _ ->
            lifeGoesOn model

        Start ->
            { model | timeIsTicking = True }

        Stop ->
            { model | timeIsTicking = False }

        AdjustSpeed adjustedSpeed ->
            { model | speed = adjustedSpeed }

        MouseClick position ->
            { model
                | text =
                    (toString position.x)
                        ++ ", "
                        ++ (toString position.y)
                        ++ " | "
                        ++ ((transformPositionToCell 1000 1000 position) |> (\p -> toString p.x ++ ", " ++ toString p.y))
                , grid = addLife model.grid position
            }


addLife : Grid -> Position -> Grid
addLife grid position =
    let
        awda =
            transformPositionToCell 1000 1000 position
    in
        if awda.x > 99 || awda.y > 99 then
            grid
        else
            grid |> indexGrid |> gridSetterGetter ( awda.x, awda.y ) |> indexGridToGrid


transformPositionToCell : Int -> Int -> Position -> Cell
transformPositionToCell w h position =
    { x = position.x // 10, y = position.y // 10 }


padDeadRow : Int -> List (List Bool)
padDeadRow n =
    List.range 1 n |> List.map (\_ -> padDeadCells 100)


padDeadCells : Int -> List Bool
padDeadCells n =
    List.range 1 n |> List.map (\_ -> False)


lifeGoesOn : Model -> Model
lifeGoesOn model =
    { model | grid = killAndSpawn model.grid }


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


gridSetter : ( Int, Int ) -> Bool -> IndexedGrid -> IndexedGrid
gridSetter ( x, y ) bool grid =
    let
        row =
            Maybe.withDefault (fromList []) <| get y grid

        updatedRow =
            set x bool row
    in
        set y updatedRow grid


gridSetterGetter : ( Int, Int ) -> IndexedGrid -> IndexedGrid
gridSetterGetter pos grid =
    gridSetter pos (not (gridGetter grid pos)) grid
