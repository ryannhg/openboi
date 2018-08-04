module Application
    exposing
        ( page
        , notFoundPage
        , basicProgram
        , docMap
        , Document
        )

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav exposing (Key)
import Html exposing (Html, text)
import Url exposing (Url)
import Url.Parser as Url


-- Pages


type alias Document msg =
    Browser.Document msg


type alias Page model route =
    { parser : Url.Parser route route
    , route : route
    , init : model
    }


page =
    Page


notFoundPage =
    Page Url.top



-- Flags


type alias Flags =
    ()



-- Model


type alias Model model =
    { key : Key
    , page : model
    }



-- Msg


type Msg msg
    = Navigation UrlMsg
    | PageMsg msg


type UrlMsg
    = OnUrlRequest UrlRequest
    | OnUrlChange Url



-- Program


type alias Config model msg route =
    { update : msg -> model -> model
    , view : model -> Document msg
    , notFoundPage : Page model route
    , pages : List (Page model route)
    }


basicProgram :
    Config model msg route
    -> Program Flags (Model model) (Msg msg)
basicProgram config =
    Browser.application
        { init = init config
        , view = view config
        , update = update config
        , subscriptions = subscriptions
        , onUrlRequest = Navigation << OnUrlRequest
        , onUrlChange = Navigation << OnUrlChange
        }


init :
    Config model msg route
    -> Flags
    -> Url
    -> Key
    -> ( Model model, Cmd (Msg msg) )
init config flags url key =
    ( Model key (getPage config url)
    , Cmd.none
    )


getPage :
    Config model msg route
    -> Url
    -> model
getPage config url =
    config.pages
        |> List.map (\page_ -> Url.map page_.route page_.parser)
        |> Url.oneOf
        |> (\routes -> Url.parse routes url)
        |> Maybe.andThen
            (\route ->
                config.pages
                    |> List.filter (\page_ -> page_.route == route)
                    |> List.head
            )
        |> Maybe.withDefault config.notFoundPage
        |> .init


view :
    Config model msg route
    -> Model model
    -> Document (Msg msg)
view config model =
    config.view model.page
        |> docMap PageMsg


docMap : (a -> b) -> Document a -> Document b
docMap fn doc =
    { title = doc.title
    , body = List.map (Html.map fn) doc.body
    }


update :
    Config model msg route
    -> Msg msg
    -> Model model
    -> ( Model model, Cmd (Msg msg) )
update config msg model =
    case msg of
        Navigation navMsg ->
            case navMsg of
                OnUrlChange url ->
                    ( { model | page = getPage config url }
                    , Cmd.none
                    )

                OnUrlRequest urlRequest ->
                    case urlRequest of
                        Internal url ->
                            ( model
                            , Nav.pushUrl model.key (Url.toString url)
                            )

                        External url ->
                            ( model
                            , Nav.load url
                            )

        PageMsg pageMsg ->
            ( { model | page = config.update pageMsg model.page }
            , Cmd.none
            )


subscriptions =
    always Sub.none
