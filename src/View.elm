module View exposing (view)

import Types exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes as H exposing (..)
import Svg exposing (..)
import Svg.Attributes as S exposing (..)
import Array exposing (..)


view : Model -> Html Msg
view model =
    div []
        [ displayModel model, buttons model.speed, Html.text model.text ]


buttons : Float -> Html Msg
buttons modelSpeed =
    div []
        [ button [ onClick (Tick 1) ] [ Html.text "Tick" ]
        , button [ onClick (Start) ] [ Html.text "Start" ]
        , button [ onClick (Stop) ] [ Html.text "Stop" ]
        , input
            [ H.type_ "range"
            , H.min "0"
            , H.max "20"
            , value <| toString modelSpeed
            , onInput <| strToFloatMsg modelSpeed
            ]
            []
        ]


strToFloatMsg : Float -> String -> Msg
strToFloatMsg modelSpeed str =
    case String.toFloat str of
        Err _ ->
            AdjustSpeed modelSpeed

        Ok val ->
            AdjustSpeed val


displayModel : Model -> Html Msg
displayModel model =
    let
        grid =
            model.grid
    in
        svg
            [ S.width "1000", S.height "1000", viewBox "0 0 100 100" ]
            ([ rect [ x "0", y "0", S.width "100", S.height "100" ] [] ]
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
        rect [ x xCoord, y yCoord, S.width "1", S.height "1", fill "#FFFFFF" ] []


isLife : ( Int, Bool ) -> Bool
isLife ( _, boolValue ) =
    boolValue
