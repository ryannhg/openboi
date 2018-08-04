module Pages.Home exposing (Model, Msg(..), init, update, view)

import Html exposing (..)


type alias Model =
    { list : List String
    }


init : Model
init =
    Model []


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


view : Model -> Html Msg
view model =
    text "Home"
