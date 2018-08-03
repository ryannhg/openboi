module Application
    exposing
        ( page
        , program
        )

import Browser
import Browser.Navigation
import Html exposing (Html)
import Url exposing (Url)
import Url.Parser as Url


type alias Config flags contextModel contextMsg pageModel pageMsg route =
    { init : flags -> Url -> ( contextModel, Cmd contextMsg )
    , pages : List (Page pageModel pageMsg route)
    }


type alias Page model msg route =
    { parser : Url.Parser
    , route : model -> route
    , init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Html msg
    }


type alias Model contextModel route =
    { context : contextModel
    , page : route
    }


type Msg contextMsg
    = ContextMsg contextMsg
    | PageMsg


program :
    Config flags contextModel contextMsg pageModel pageMsg route
    -> Program flags (Model contextModel route) (Msg contextMsg)
program config =
    Browser.application
        { init = init config
        , view = view config
        , update = update config
        , subscriptions = subscriptions config
        , onUrlRequest = onUrlRequest config
        , onUrlChange = onUrlChange config
        }
