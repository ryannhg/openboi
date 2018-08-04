module Main exposing (main)

import Application
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


type PageModel
    = HomeModel Home.Model
    | ScheduleModel Schedule.Model
    | TimesheetsModel Timesheets.Model
    | ExpensesModel Expenses.Model
    | NotFoundModel NotFound.Model


type PageMsg
    = HomeMsg Home.Msg
    | ScheduleMsg Schedule.Msg
    | TimesheetsMsg Timesheets.Msg
    | ExpensesMsg Expenses.Msg
    | NotFoundMsg NotFound.Msg



-- Update


update : PageMsg -> PageModel -> PageModel
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



-- Main


main =
    Application.basicProgram
        { update = update
        , notFoundPage =
            Application.notFoundPage
                NotFound
                NotFoundModel
                NotFoundMsg
                "Not Found"
                NotFound.init
                NotFound.update
                NotFound.view
        , pages =
            [ Application.page
                Url.top
                Home
                HomeModel
                HomeMsg
                "Home"
                Home.init
                Home.update
                Home.view
            , Application.page
                (Url.s "schedule")
                Schedule
                ScheduleModel
                ScheduleMsg
                "Schedule"
                Schedule.init
                Schedule.update
                Schedule.view
            , Application.page
                (Url.s "timesheets")
                Timesheets
                TimesheetsModel
                TimesheetsMsg
                "Timesheets"
                Timesheets.init
                Timesheets.update
                Timesheets.view
            , Application.page
                (Url.s "expenses")
                Expenses
                ExpensesModel
                ExpensesMsg
                "Expenses"
                Expenses.init
                Expenses.update
                Expenses.view
            ]
        }
