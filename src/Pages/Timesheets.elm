module Pages.Timesheets
    exposing
        ( Model
        , Msg(..)
        , init
        , update
        , view
        , subscriptions
        )

import Application exposing (Session)
import Context
import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)


type alias Model =
    { list : List String
    }


init : Session Context.Model -> ( Model, Cmd Msg, Cmd Context.Msg )
init session =
    ( Model []
    , Cmd.none
    , Cmd.none
    )


type Msg
    = NoOp


update : Application.Session Context.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Context.Msg )
update session msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            , Cmd.none
            )


view : Application.Session Context.Model -> Model -> Document Msg
view session model =
    { title = "Timesheets"
    , body =
        [ h1 [] [ text "Timesheets" ]
        ]
    }


subscriptions : Application.Session Context.Model -> Model -> Sub Msg
subscriptions session model =
    Sub.none
