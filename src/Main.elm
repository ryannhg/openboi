module Main exposing (main)

import Application
import Pages.Home as Home
import Pages.Schedule as Schedule
import Pages.Timesheets as Timesheets
import Pages.Expenses as Expenses
import Url.Parser as Url


-- Types


type Route
    = Home Home.Model
    | Schedule Schedule.Model
    | Timesheets Timesheets.Model
    | Expenses Expenses.Model



-- Main


main : Program Never Model Msg
main =
    Application.program
        [ Application.page
            Url.top
            Home
            Home.init
            Home.update
            Home.view
        , Application.page
            (Url.s "schedule")
            Schedule
            Schedule.init
            Schedule.update
            Schedule.view
        , Application.page
            (Url.s "timesheets")
            Timesheets
            Timesheets.init
            Timesheets.update
            Timesheets.view
        , Application.page
            (Url.s "expenses")
            Expenses
            Expenses.init
            Expenses.update
            Expenses.view
        ]
