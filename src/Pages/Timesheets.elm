module Pages.Timesheets
    exposing
        ( Model
        , Msg(..)
        , init
        , update
        , view
        , subscriptions
        )

import Context
import Browser exposing (Document)
import Html exposing (..)


type alias Model =
    { list : List String
    }


init : Context.Model -> ( Model, Cmd Msg, Cmd Context.Msg )
init context =
    ( Model []
    , Cmd.none
    , Cmd.none
    )


type Msg
    = NoOp


update : Context.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Context.Msg )
update context msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            , Cmd.none
            )


view : Context.Model -> Model -> Document Msg
view context model =
    { title = "Timesheets"
    , body =
        [ h1 [] [ text "Timesheets" ]
        ]
    }


subscriptions : Context.Model -> Model -> Sub Msg
subscriptions context model =
    Sub.none
