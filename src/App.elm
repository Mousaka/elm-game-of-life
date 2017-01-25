module App exposing (..)

import State exposing (update, init)
import Types exposing (Model, Msg(Tick, MouseClick))
import View exposing (view)
import Html exposing (program)
import Time exposing (every, millisecond)
import Mouse exposing (clicks)


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, subscriptions = subscriptions, update = update }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ timeSub model, mouseSub ]


timeSub : Model -> Sub Msg
timeSub model =
    if model.timeIsTicking && not (model.speed == 0) then
        every (millisecond * (500 / (model.speed + 0.5))) Tick
    else
        Sub.none


mouseSub : Sub Msg
mouseSub =
    Mouse.clicks MouseClick
