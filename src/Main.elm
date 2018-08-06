module Main exposing (main)

import Application exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Elements.Navbar
import Elements.Navigation
import Pages.Home as Home
import Pages.Schedule as Schedule
import Pages.Timesheets as Timesheets
import Pages.Expenses as Expenses
import Pages.NotFound as NotFound
import Url.Parser as Url
import Context


-- Routes, Models, Msgs


type Route
    = Home
    | Schedule
    | Timesheets
    | Expenses
    | NotFound


type Model
    = HomeModel Home.Model
    | ScheduleModel Schedule.Model
    | TimesheetsModel Timesheets.Model
    | ExpensesModel Expenses.Model
    | NotFoundModel NotFound.Model


type Msg
    = HomeMsg Home.Msg
    | ScheduleMsg Schedule.Msg
    | TimesheetsMsg Timesheets.Msg
    | ExpensesMsg Expenses.Msg
    | NotFoundMsg NotFound.Msg



-- Init


init : Route -> Application.Session Context.Model -> ( Model, Cmd Msg, Cmd Context.Msg )
init route =
    case route of
        Home ->
            Application.initPage HomeModel HomeMsg Home.init

        Schedule ->
            Application.initPage ScheduleModel ScheduleMsg Schedule.init

        Timesheets ->
            Application.initPage TimesheetsModel TimesheetsMsg Timesheets.init

        Expenses ->
            Application.initPage ExpensesModel ExpensesMsg Expenses.init

        NotFound ->
            Application.initPage NotFoundModel NotFoundMsg NotFound.init



-- Update


update : Msg -> Model -> Application.Session Context.Model -> ( Model, Cmd Msg, Cmd Context.Msg )
update msg_ model_ =
    case ( msg_, model_ ) of
        ( HomeMsg msg, HomeModel model ) ->
            Application.updatePage msg model HomeModel HomeMsg Home.update

        ( ScheduleMsg msg, ScheduleModel model ) ->
            Application.updatePage msg model ScheduleModel ScheduleMsg Schedule.update

        ( TimesheetsMsg msg, TimesheetsModel model ) ->
            Application.updatePage msg model TimesheetsModel TimesheetsMsg Timesheets.update

        ( ExpensesMsg msg, ExpensesModel model ) ->
            Application.updatePage msg model ExpensesModel ExpensesMsg Expenses.update

        ( NotFoundMsg msg, NotFoundModel model ) ->
            Application.updatePage msg model NotFoundModel NotFoundMsg NotFound.update

        ( _, _ ) ->
            (\_ -> ( model_, Cmd.none, Cmd.none ))



-- View


view : Model -> Application.Session Context.Model -> Document Msg
view model_ =
    case model_ of
        HomeModel model ->
            Application.viewPage model HomeMsg Home.view

        ScheduleModel model ->
            Application.viewPage model ScheduleMsg Schedule.view

        TimesheetsModel model ->
            Application.viewPage model TimesheetsMsg Timesheets.view

        ExpensesModel model ->
            Application.viewPage model ExpensesMsg Expenses.view

        NotFoundModel model ->
            Application.viewPage model NotFoundMsg NotFound.view


viewWrapper : Application.Session Context.Model -> Document Msg -> Document Msg
viewWrapper session { title, body } =
    { title = title
    , body =
        [ div
            [ class "h-100 transition"
            , classList
                [ ( "transition--notready", session.transition == Application.NotReady )
                ]
            ]
            [ div [] [ Elements.Navbar.view session ]
            , div [ class "container h-100" ]
                [ div [ class "row h-100" ]
                    [ Elements.Navigation.view session
                    , main_ [ class "col h-100 pad-lg pad-l-xl" ]
                        [ div
                            [ class "transition"
                            , classList
                                [ ( "transition--leaving", session.transition == Application.Leaving )
                                , ( "transition--entering", session.transition == Application.Entering )
                                ]
                            ]
                            body
                        ]
                    ]
                ]
            ]
        ]
    }



-- Main


main =
    Application.program
        { context =
            { init = Context.init
            , update = Context.update
            }
        , init = init
        , update = update
        , view = (\model session -> view model session |> (viewWrapper session))
        , subscriptions = (\session -> always Sub.none)
        , notFoundPage =
            Application.notFoundPage
                NotFound
        , pages =
            [ Application.page
                Url.top
                Home
            , Application.page
                (Url.s "schedule")
                Schedule
            , Application.page
                (Url.s "timesheets")
                Timesheets
            , Application.page
                (Url.s "expenses")
                Expenses
            ]
        }
