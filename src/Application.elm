module Application
    exposing
        ( page
        , notFoundPage
        , basicProgram
        )

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav exposing (Key)
import Html exposing (Html, text)
import Url exposing (Url)
import Url.Parser as Url


-- Pages


type State model
    = Inactive
    | Active model


type alias Page bigModel bigMsg smallModel smallMsg route =
    { -- Internals
      state : State bigModel

    -- Routing
    , parser : Url.Parser route route
    , route : route

    -- Type Normalization
    , toModel : smallModel -> bigModel
    , toMsg : smallMsg -> bigMsg

    -- Page Functions
    , title : String
    , init : smallModel
    , update : smallMsg -> smallModel -> smallModel
    , view : smallModel -> Html smallMsg
    }


page =
    Page Inactive


notFoundPage =
    Page Inactive Url.top



-- Flags


type alias Flags =
    ()



-- Model


type alias Model bigModel bigMsg smallModel smallMsg route =
    { key : Key
    , page : Page bigModel bigMsg smallModel smallMsg route
    }



-- Msg


type Msg bigMsg
    = Navigation UrlMsg
    | PageMsg bigMsg


type UrlMsg
    = OnUrlRequest UrlRequest
    | OnUrlChange Url



-- Program


type alias Config bigModel bigMsg smallModel smallMsg route =
    { update : bigMsg -> bigModel -> bigModel
    , notFoundPage : Page bigModel bigMsg smallModel smallMsg route
    , pages : List (Page bigModel bigMsg smallModel smallMsg route)
    }


basicProgram :
    Config bigModel bigMsg smallModel smallMsg route
    -> Program Flags (Model bigModel bigMsg smallModel smallMsg route) (Msg bigMsg)
basicProgram config =
    Browser.application
        { init = init config
        , view = view
        , update = update config
        , subscriptions = subscriptions
        , onUrlRequest = Navigation << OnUrlRequest
        , onUrlChange = Navigation << OnUrlChange
        }


init :
    Config bigModel bigMsg smallModel smallMsg route
    -> Flags
    -> Url
    -> Key
    -> ( Model bigModel bigMsg smallModel smallMsg route, Cmd (Msg bigMsg) )
init config flags url key =
    ( Model key (getPage config url)
    , Cmd.none
    )


getPage :
    Config bigModel bigMsg smallModel smallMsg route
    -> Url
    -> Page bigModel bigMsg smallModel smallMsg route
getPage config url =
    (Url.parse
        (config.pages
            |> List.map (\page_ -> Url.map page_.route page_.parser)
            |> Url.oneOf
        )
        url
    )
        |> Maybe.andThen
            (\route ->
                config.pages
                    |> List.filter (\page_ -> page_.route == route)
                    |> List.head
            )
        |> Maybe.withDefault config.notFoundPage
        |> (\page_ -> { page_ | state = Active (page_.toModel page_.init) })


view :
    Model bigModel bigMsg smallModel smallMsg route
    -> Document (Msg bigMsg)
view model =
    Document
        model.page.title
        [ case model.page.state of
            Active pageModel ->
                Html.map (PageMsg << model.page.toMsg) (model.page.view pageModel)

            Inactive ->
                text "Critical error (the page state is inactive)!"
        ]


update :
    Config bigModel bigMsg smallModel smallMsg route
    -> Msg bigMsg
    -> Model bigModel bigMsg smallModel smallMsg route
    -> ( Model bigModel bigMsg smallModel smallMsg route, Cmd (Msg bigMsg) )
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
            ( { model | page = updatePageState config pageMsg model.page }
            , Cmd.none
            )


updatePageState :
    Config bigModel bigMsg smallModel smallMsg route
    -> bigMsg
    -> Page bigModel bigMsg smallModel smallMsg route
    -> Page bigModel bigMsg smallModel smallMsg route
updatePageState config pageMsg page_ =
    case page_.state of
        Active pageModel ->
            { page_ | state = Active <| config.msgUnwrapper pageMsg pageModel }

        Inactive ->
            let
                _ =
                    Debug.log "Page bigModel bigMsg state is inactive!" page_
            in
                page_


subscriptions =
    always Sub.none
