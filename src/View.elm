module View exposing (view)

import Types exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Array exposing (..)


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
        indexedList =
            gridToIndexedList grid

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
