module Pages.NotFound exposing (Model, Msg(..), init, update, view)

import Browser exposing (Document)
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


view : Model -> Document Msg
view model =
    { title = "Not Found"
    , body =
        [ text "Not Found"
        ]
    }
