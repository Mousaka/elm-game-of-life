module App exposing (..)

import State exposing (update, init)
import Types exposing (Model, Msg(Tick))
import View exposing (view)
import Html exposing (program)
import Time exposing (every, millisecond)


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, subscriptions = subscriptions, update = update }


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.timeIsTicking && not (model.speed == 0) then
        every (millisecond * (500 / (model.speed + 0.5))) Tick
    else
        Sub.none
