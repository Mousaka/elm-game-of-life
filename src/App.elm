module App exposing (..)

import State exposing (update, model)
import Types exposing (Model, Msg)
import View exposing (view)
import Html exposing (beginnerProgram)


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }
