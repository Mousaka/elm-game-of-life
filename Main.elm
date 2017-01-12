module Main exposing (..)

import Array exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Grid =
    Array (Array Bool)


type alias Model =
    { grid : Grid
    , text : String
    }


model : Model
model =
    Model
        (Array.fromList
            ([ Array.fromList <| [ False, True, False ] ++ padDead
             , Array.fromList <| [ False, False, True ] ++ padDead
             , Array.fromList <| [ True, True, True ] ++ padDead
             ]
                ++ List.map (\_ -> (Array.fromList padDead)) (List.range 1 47)
            )
        )
        "hej"



-- UPDATE


type Msg
    = Tick


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick ->
            lifeGoesOn model


lifeGoesOn : Model -> Model
lifeGoesOn model =
    { model | text = model.text ++ "!", grid = killAndSpawn model.grid }


killAndSpawn : Grid -> Grid
killAndSpawn grid =
    -- [(0, Array Bool) ... ]
    fromIndexedList (List.map (killAndSpawnRow grid) (toIndexedList grid))


killAndSpawnRow : Grid -> ( Int, Array Bool ) -> ( Int, Array Bool )
killAndSpawnRow grid ( y, row ) =
    let
        newRow =
            fromIndexedList (deathRow grid y (toIndexedList row))
    in
        ( y, newRow )


fromIndexedList : List ( Int, a ) -> Array a
fromIndexedList list =
    fromList <| List.map (\( a, b ) -> b) list


deathRow : Grid -> Int -> List ( Int, Bool ) -> List ( Int, Bool )
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


countLivingNeighbours : Grid -> Int -> Int -> Int
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
        List.length <| List.filter (checkIfAlive grid) ncoords


checkIfAlive : Grid -> ( Int, Int ) -> Bool
checkIfAlive grid ( x, y ) =
    gridGetter grid x y


gridGetter : Grid -> Int -> Int -> Bool
gridGetter grid x y =
    let
        row =
            Maybe.withDefault (fromList []) <| get y grid
    in
        Maybe.withDefault False <| get x row



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ displayModel model, button [ onClick Tick ] [ Html.text "Tick" ], Html.text model.text ]


displayModel : Model -> Html Msg
displayModel model =
    let
        grid =
            model.grid
    in
        svg
            [ width "750", height "750", viewBox "0 0 100 100" ]
            ([ rect [ x "0", y "0", width "100", height "100" ] [] ]
                ++ createRows grid
            )


createRows : Grid -> List (Html Msg)
createRows grid =
    let
        y =
            0

        indexedList =
            grid
                -- Ex: grid = arr [ arr [ True ], arr [ False, True ] ]
                |>
                    toIndexedList
                -- list [ (0, arr [ True ]), (1, arr [ False, True ]) ]
                |>
                    List.map indexListUtil

        -- list [ (0, list [ (0, True) ]), (1, list [ (0, False), (1, True) ] ) ]
    in
        List.concatMap createRow indexedList


indexListUtil : ( Int, Array Bool ) -> ( Int, List ( Int, Bool ) )
indexListUtil ( x, arr ) =
    ( x, toIndexedList arr )


createRow : ( Int, List ( Int, Bool ) ) -> List (Html Msg)
createRow ( y, row ) =
    row
        |> List.filter isLife
        |> List.map (toBox y)


toBox : Int -> ( Int, Bool ) -> Svg Msg
toBox yVar ( xVar, _ ) =
    let
        xCoord =
            toString xVar

        yCoord =
            toString yVar
    in
        rect [ x xCoord, y yCoord, width "1", height "1", fill "#FFFFFF" ] []


isLife : ( Int, Bool ) -> Bool
isLife ( _, boolValue ) =
    boolValue


padDead : List Bool
padDead =
    [ False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    ]
