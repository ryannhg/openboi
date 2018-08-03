module Home exposing (Model, Msg(..), init, update, view)

import Html exposing (..)


type alias Model =
    { list : List String
    }


init : ( Model, Cmd Msg )
init =
    ( Model []
    , Cmd.none
    )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model


view : Model -> Html Msg
view model =
    text "Home"
