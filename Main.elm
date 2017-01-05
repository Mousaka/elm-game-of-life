module Main exposing (..)

import Array exposing (..)
import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Seed =
    Array (Array Bool)


type alias Model =
    { seed : Seed
    }


model : Model
model =
    Model (Array.fromList [ Array.fromList [ True, True, False, True ], Array.fromList [ False, False, True, False, False, True, True ] ])



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
    model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ displayModel model ]


displayModel : Model -> Html Msg
displayModel model =
    let
        seed =
            model.seed
    in
        svg
            [ width "750", height "750", viewBox "0 0 100 100" ]
            ([ rect [ x "0", y "0", width "100", height "100" ] [] ]
                ++ createRows seed
            )


createRows : Seed -> List (Html Msg)
createRows seed =
    let
        y =
            0

        indexedList =
            seed
                -- Ex: seed = arr [ arr [ True ], arr [ False, True ] ]
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
