module Application
    exposing
        ( page
        , notFoundPage
        , program
        , viewPage
        , updatePage
        , initPage
        , Document
        , Transition(..)
        )

import Utilities exposing (delayedCommand)
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav exposing (Key)
import Html exposing (Html, text)
import Url exposing (Url)
import Url.Parser as Url


transitionDelay : Float
transitionDelay =
    400


type Transition
    = NotReady
    | Entering
    | Ready
    | Leaving



-- Pages


type alias Contextual a =
    { a | url : Url, transition : Transition }


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
    = SetTransition Transition
    | Navigation UrlMsg
    | ContextMsg contextMsg
    | PageMsg msg


type UrlMsg
    = OnUrlRequest UrlRequest
    | OnUrlChange Url
    | PushUrl Url Key



-- Program


type alias Config flags contextModel contextMsg model msg route =
    { context : ContextConfig flags (Contextual contextModel) contextMsg
    , init : route -> Contextual contextModel -> ( model, Cmd msg, Cmd contextMsg )
    , update : msg -> model -> Contextual contextModel -> ( model, Cmd msg, Cmd contextMsg )
    , view : model -> Contextual contextModel -> Document msg
    , subscriptions : Contextual contextModel -> model -> Sub msg
    , notFoundPage : Page route
    , pages : List (Page route)
    }


type alias ContextConfig flags model msg =
    { init : flags -> Url -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    }


program :
    Config flags contextModel contextMsg model msg route
    -> Program flags (Model (Contextual contextModel) model) (Msg contextMsg msg)
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
    -> ( Model (Contextual contextModel) model, Cmd (Msg contextMsg msg) )
init config flags url key =
    let
        ( contextModel, contextCmd ) =
            config.context.init flags url

        ( pageModel, pageCmd, pageContextCmd ) =
            config.init (getRoute config url) contextModel
    in
        ( Model
            key
            contextModel
            pageModel
        , Cmd.batch
            [ delayedCommand transitionDelay (SetTransition Ready)
            , Cmd.map ContextMsg contextCmd
            , Cmd.map ContextMsg pageContextCmd
            , Cmd.map PageMsg pageCmd
            ]
        )


initPage :
    (pageModel -> model)
    -> (pageMsg -> msg)
    -> (Contextual contextModel -> ( pageModel, Cmd pageMsg, Cmd contextMsg ))
    -> Contextual contextModel
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
    -> Model (Contextual contextModel) model
    -> Document (Msg contextMsg msg)
view config model =
    docMap PageMsg (config.view model.page model.context)


viewPage :
    pageModel
    -> (pageMsg -> msg)
    -> (Contextual contextModel -> pageModel -> Document pageMsg)
    -> Contextual contextModel
    -> Document msg
viewPage model toMsg view_ contextModel =
    docMap toMsg (view_ contextModel model)



-- Update


updatePage :
    pageMsg
    -> pageModel
    -> (pageModel -> model)
    -> (pageMsg -> msg)
    -> (Contextual contextModel -> pageMsg -> pageModel -> ( pageModel, Cmd pageMsg, Cmd contextMsg ))
    -> Contextual contextModel
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
    -> Model (Contextual contextModel) model
    -> ( Model (Contextual contextModel) model, Cmd (Msg contextMsg msg) )
update config msg model =
    case msg of
        SetTransition transition ->
            let
                context =
                    model.context
            in
                ( { model | context = { context | transition = transition } }
                , Cmd.none
                )

        Navigation navMsg ->
            case navMsg of
                OnUrlChange url ->
                    let
                        ( pageModel, pageCmd, contextCmd ) =
                            config.init (getRoute config url) model.context

                        context =
                            model.context
                    in
                        ( { model
                            | page = pageModel
                            , context = { context | url = url, transition = Entering }
                          }
                        , Cmd.batch
                            [ Cmd.map ContextMsg contextCmd
                            , Cmd.map PageMsg pageCmd
                            , delayedCommand transitionDelay (SetTransition Ready)
                            ]
                        )

                PushUrl url key ->
                    ( model
                    , Nav.pushUrl key (Url.toString url)
                    )

                OnUrlRequest urlRequest ->
                    case urlRequest of
                        Internal url ->
                            if url == model.context.url || model.context.transition /= Ready then
                                ( model, Cmd.none )
                            else
                                let
                                    context =
                                        model.context
                                in
                                    ( { model | context = { context | transition = Leaving } }
                                    , delayedCommand transitionDelay (Navigation <| PushUrl url model.key)
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
    -> Model (Contextual contextModel) model
    -> Sub (Msg contextMsg msg)
subscriptions config =
    always Sub.none
