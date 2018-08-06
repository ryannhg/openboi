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
        , Session
        )

import Utilities exposing (delayedCommand)
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav exposing (Key)
import Html exposing (Html, text)
import Url exposing (Url)
import Url.Parser as Url


-- Transitions


transitionDelay : Float
transitionDelay =
    400


type Transition
    = NotReady
    | Entering
    | Ready
    | Leaving



-- Pages


type alias Session contextModel =
    { url : Url
    , transition : Transition
    , context : contextModel
    }


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
    , session : Session contextModel
    , page : model
    }



-- Program


type alias Config flags contextModel contextMsg model msg route =
    { context : ContextConfig flags contextModel contextMsg
    , init : route -> Session contextModel -> ( model, Cmd msg, Cmd contextMsg )
    , update : msg -> model -> Session contextModel -> ( model, Cmd msg, Cmd contextMsg )
    , view : model -> Session contextModel -> Document msg
    , subscriptions : Session contextModel -> model -> Sub msg
    , notFoundPage : Page route
    , pages : List (Page route)
    }


type alias ContextConfig flags model msg =
    { init : flags -> Url -> ( model, Cmd msg )
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
            config.context.init flags url

        session =
            Session url NotReady contextModel

        ( pageModel, pageCmd, pageContextCmd ) =
            config.init (getRoute config url) session
    in
        ( Model
            key
            session
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
    -> (Session contextModel -> ( pageModel, Cmd pageMsg, Cmd contextMsg ))
    -> Session contextModel
    -> ( model, Cmd msg, Cmd contextMsg )
initPage toModel toMsg init_ session =
    let
        ( updatedPageModel, updatedPageCmd, updatedContextCmd ) =
            (init_ session)
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
    docMap PageMsg (config.view model.page model.session)


viewPage :
    pageModel
    -> (pageMsg -> msg)
    -> (Session contextModel -> pageModel -> Document pageMsg)
    -> Session contextModel
    -> Document msg
viewPage model toMsg view_ contextModel =
    docMap toMsg (view_ contextModel model)



-- Update


type Msg contextMsg msg
    = SetTransition Transition
    | Navigation UrlMsg
    | ContextMsg contextMsg
    | PageMsg msg


type UrlMsg
    = OnUrlRequest UrlRequest
    | OnUrlChange Url
    | PushUrl Url Key


updatePage :
    pageMsg
    -> pageModel
    -> (pageModel -> model)
    -> (pageMsg -> msg)
    -> (Session contextModel -> pageMsg -> pageModel -> ( pageModel, Cmd pageMsg, Cmd contextMsg ))
    -> Session contextModel
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
        SetTransition transition ->
            let
                session =
                    model.session
            in
                ( { model | session = { session | transition = transition } }
                , Cmd.none
                )

        Navigation navMsg ->
            case navMsg of
                OnUrlChange url ->
                    let
                        ( pageModel, pageCmd, contextCmd ) =
                            config.init (getRoute config url) model.session

                        session =
                            model.session
                    in
                        ( { model
                            | page = pageModel
                            , session = { session | url = url, transition = Entering }
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
                            if url == model.session.url || model.session.transition /= Ready then
                                ( model, Cmd.none )
                            else
                                let
                                    session =
                                        model.session
                                in
                                    ( { model | session = { session | transition = Leaving } }
                                    , delayedCommand transitionDelay (Navigation <| PushUrl url model.key)
                                    )

                        External url ->
                            ( model
                            , Nav.load url
                            )

        ContextMsg contextMsg ->
            let
                ( updatedContext, updatedContextCmd ) =
                    config.context.update contextMsg model.session.context

                session =
                    model.session
            in
                ( { model | session = { session | context = updatedContext } }
                , Cmd.map ContextMsg updatedContextCmd
                )

        PageMsg pageMsg ->
            let
                ( updatedPageModel, updatedPageCmd, updatedContextCmd ) =
                    config.update pageMsg model.page model.session
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
