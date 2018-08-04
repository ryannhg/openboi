module Main exposing (main)

import Application exposing (Document, docMap)
import Html exposing (..)
import Html.Attributes exposing (..)
import Pages.Home as Home
import Pages.Schedule as Schedule
import Pages.Timesheets as Timesheets
import Pages.Expenses as Expenses
import Pages.NotFound as NotFound
import Url.Parser as Url


-- Routes


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



-- Update


update : Msg -> Model -> Model
update msg model =
    case ( msg, model ) of
        ( HomeMsg msg_, HomeModel model_ ) ->
            HomeModel <| Home.update msg_ model_

        ( ScheduleMsg msg_, ScheduleModel model_ ) ->
            ScheduleModel <| Schedule.update msg_ model_

        ( TimesheetsMsg msg_, TimesheetsModel model_ ) ->
            TimesheetsModel <| Timesheets.update msg_ model_

        ( ExpensesMsg msg_, ExpensesModel model_ ) ->
            ExpensesModel <| Expenses.update msg_ model_

        ( NotFoundMsg msg_, NotFoundModel model_ ) ->
            NotFoundModel <| NotFound.update msg_ model_

        ( _, _ ) ->
            model


view : Model -> Document Msg
view model =
    viewWrapper <|
        case model of
            HomeModel model_ ->
                Home.view model_ |> docMap HomeMsg

            ScheduleModel model_ ->
                Schedule.view model_ |> docMap ScheduleMsg

            TimesheetsModel model_ ->
                Timesheets.view model_ |> docMap TimesheetsMsg

            ExpensesModel model_ ->
                Expenses.view model_ |> docMap ExpensesMsg

            NotFoundModel model_ ->
                NotFound.view model_ |> docMap NotFoundMsg


viewWrapper : Document Msg -> Document Msg
viewWrapper { title, body } =
    { title = title
    , body =
        [ div []
            [ a [ href "/" ] [ text "Home" ]
            , a [ style "margin-left" ".5rem", href "/schedule" ] [ text "Schedule" ]
            , a [ style "margin-left" ".5rem", href "/timesheets" ] [ text "Timesheets" ]
            , a [ style "margin-left" ".5rem", href "/expenses" ] [ text "Expenses" ]
            , a [ style "margin-left" ".5rem", href "/garbage-town" ] [ text "Bad link" ]
            ]
        , div [] body
        ]
    }



-- Main


main =
    Application.basicProgram
        { update = update
        , view = view
        , notFoundPage =
            Application.notFoundPage
                NotFound
                (NotFoundModel NotFound.init)
        , pages =
            [ Application.page
                Url.top
                Home
                (HomeModel Home.init)
            , Application.page
                (Url.s "schedule")
                Schedule
                (ScheduleModel Schedule.init)
            , Application.page
                (Url.s "timesheets")
                Timesheets
                (TimesheetsModel Timesheets.init)
            , Application.page
                (Url.s "expenses")
                Expenses
                (ExpensesModel Expenses.init)
            ]
        }
