module Application
    exposing
        ( page
        , notFoundPage
        , program
        , viewPage
        , updatePage
        , initPage
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


type alias Page route =
    { parser : Url.Parser route route
    , route : route
    }


page : Url.Parser route route -> route -> Page route
page =
    Page


notFoundPage : route -> Page route
notFoundPage =
    Page Url.top



-- Model


type alias Model contextModel model =
    { key : Key
    , context : contextModel
    , page : model
    }



-- Msg


type Msg contextMsg msg
    = Navigation UrlMsg
    | ContextMsg contextMsg
    | PageMsg msg


type UrlMsg
    = OnUrlRequest UrlRequest
    | OnUrlChange Url



-- Program


type alias Config flags contextModel contextMsg model msg route =
    { context : ContextConfig flags contextModel contextMsg
    , init : route -> contextModel -> ( model, Cmd msg, Cmd contextMsg )
    , update : msg -> model -> contextModel -> ( model, Cmd msg, Cmd contextMsg )
    , view : model -> contextModel -> Document msg
    , subscriptions : contextModel -> model -> Sub msg
    , notFoundPage : Page route
    , pages : List (Page route)
    }


type alias ContextConfig flags model msg =
    { init : flags -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    }


program :
    Config flags contextModel contextMsg model msg route
    -> Program flags (Model contextModel model) (Msg contextMsg msg)
program config =
    Browser.application
        { init = init config
        , view = view config
        , update = update config
        , subscriptions = subscriptions config
        , onUrlRequest = Navigation << OnUrlRequest
        , onUrlChange = Navigation << OnUrlChange
        }



-- Init


init :
    Config flags contextModel contextMsg model msg route
    -> flags
    -> Url
    -> Key
    -> ( Model contextModel model, Cmd (Msg contextMsg msg) )
init config flags url key =
    let
        ( contextModel, contextCmd ) =
            config.context.init flags

        ( pageModel, pageCmd, pageContextCmd ) =
            config.init (getRoute config url) contextModel
    in
        ( Model
            key
            contextModel
            pageModel
        , Cmd.batch
            [ Cmd.map ContextMsg contextCmd
            , Cmd.map ContextMsg pageContextCmd
            , Cmd.map PageMsg pageCmd
            ]
        )


initPage :
    (pageModel -> model)
    -> (pageMsg -> msg)
    -> (contextModel -> ( pageModel, Cmd pageMsg, Cmd contextMsg ))
    -> contextModel
    -> ( model, Cmd msg, Cmd contextMsg )
initPage toModel toMsg init_ contextModel =
    let
        ( updatedPageModel, updatedPageCmd, updatedContextCmd ) =
            (init_ contextModel)
    in
        ( toModel updatedPageModel
        , Cmd.map toMsg updatedPageCmd
        , updatedContextCmd
        )


getRoute :
    Config flags contextModel contextMsg model msg route
    -> Url
    -> route
getRoute config url =
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
        |> .route



-- View


docMap : (a -> b) -> Document a -> Document b
docMap toMsg doc =
    { title = doc.title
    , body = List.map (Html.map toMsg) doc.body
    }


view :
    Config flags contextModel contextMsg model msg route
    -> Model contextModel model
    -> Document (Msg contextMsg msg)
view config model =
    docMap PageMsg (config.view model.page model.context)


viewPage :
    pageModel
    -> (pageMsg -> msg)
    -> (contextModel -> pageModel -> Document pageMsg)
    -> contextModel
    -> Document msg
viewPage model toMsg view_ contextModel =
    docMap toMsg (view_ contextModel model)



-- Update


updatePage :
    pageMsg
    -> pageModel
    -> (pageModel -> model)
    -> (pageMsg -> msg)
    -> (contextModel -> pageMsg -> pageModel -> ( pageModel, Cmd pageMsg, Cmd contextMsg ))
    -> contextModel
    -> ( model, Cmd msg, Cmd contextMsg )
updatePage msg model toModel toMsg update_ contextModel =
    let
        ( updatedPageModel, updatedPageCmd, updatedContextCmd ) =
            (update_ contextModel msg model)
    in
        ( toModel updatedPageModel
        , Cmd.map toMsg updatedPageCmd
        , updatedContextCmd
        )


update :
    Config flags contextModel contextMsg model msg route
    -> Msg contextMsg msg
    -> Model contextModel model
    -> ( Model contextModel model, Cmd (Msg contextMsg msg) )
update config msg model =
    case msg of
        Navigation navMsg ->
            case navMsg of
                OnUrlChange url ->
                    let
                        ( pageModel, pageCmd, contextCmd ) =
                            config.init (getRoute config url) model.context
                    in
                        ( { model | page = pageModel }
                        , Cmd.batch
                            [ Cmd.map ContextMsg contextCmd
                            , Cmd.map PageMsg pageCmd
                            ]
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

        ContextMsg contextMsg ->
            let
                ( updatedContext, updatedContextCmd ) =
                    config.context.update contextMsg model.context
            in
                ( { model | context = updatedContext }
                , Cmd.map ContextMsg updatedContextCmd
                )

        PageMsg pageMsg ->
            let
                ( updatedPageModel, updatedPageCmd, updatedContextCmd ) =
                    config.update pageMsg model.page model.context
            in
                ( { model | page = updatedPageModel }
                , Cmd.batch
                    [ Cmd.map PageMsg updatedPageCmd
                    , Cmd.map ContextMsg updatedContextCmd
                    ]
                )



-- Subscriptions
-- TODO: Make these do something.


subscriptions :
    Config flags contextModel contextMsg model msg route
    -> Model contextModel model
    -> Sub (Msg contextMsg msg)
subscriptions config =
    always Sub.none
