module Context.Functions
    exposing
        ( update
        , init
        , view
        )

import Application
import Url exposing (Url)
import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Elements.Navbar
import Elements.Navigation
import Context exposing (..)


init : Flags -> Url -> ( Model, Cmd Msg )
init flags url =
    ( Model
        (Just <| User "Ryan" "ryan.nhg@gmail.com")
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetUser user ->
            ( { model | user = Just user }
            , Cmd.none
            )

        RemoveUser ->
            ( { model | user = Nothing }
            , Cmd.none
            )


view :
    (Msg -> Application.Msg Msg pageMsg)
    -> (pageMsg -> Application.Msg Msg pageMsg)
    -> Application.Session Model
    -> Document pageMsg
    -> Document (Application.Msg Msg pageMsg)
view fromContextMsg fromPageMsg session pageDocument =
    { title = pageDocument.title
    , body =
        [ div
            [ class "h-100 transition"
            , classList
                [ ( "transition--notready", session.transition == Application.NotReady )
                ]
            ]
            [ div [] [ Html.map fromContextMsg <| Elements.Navbar.view session ]
            , div [ class "container h-100" ]
                [ div [ class "row h-100" ]
                    [ Html.map fromContextMsg <| Elements.Navigation.view session
                    , main_ [ class "col h-100 pad-lg pad-l-xl" ]
                        [ div
                            [ class "transition"
                            , classList
                                [ ( "transition--leaving", session.transition == Application.Leaving )
                                , ( "transition--entering", session.transition == Application.Entering )
                                ]
                            ]
                            (List.map (Html.map fromPageMsg) pageDocument.body)
                        ]
                    ]
                ]
            ]
        ]
    }
