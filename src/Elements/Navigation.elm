module Elements.Navigation exposing (view, links, Link)

import Context
import Html exposing (..)
import Html.Attributes exposing (..)
import Url exposing (Url)


type alias Link =
    { url : String
    , label : String
    }


links : List Link
links =
    [ Link "/schedule" "Schedule"
    , Link "/timesheets" "Timesheets"
    , Link "/expenses" "Expenses"
    ]


view : Context.Model -> Html msg
view context =
    div
        [ class "col-auto h-100 pad-xl hide-sm-down"
        , style "box-shadow" "16px 0 16px rgba(0, 0, 0, .10)"
        ]
        [ aside [ class "navigation text-center pad-x-sm" ]
            [ h3 [ class "pad-0" ]
                [ viewLink context.url (Link "/" "OpenBoi")
                ]
            , div [ class "tabs" ]
                [ nav [ class "tabs-nav tabs-nav-block pad-t-lg" ]
                    (List.map (viewLink context.url) links)
                ]
            ]
        ]


viewLink : Url -> Link -> Html msg
viewLink url link =
    a
        [ href link.url
        , classList (tabClasses (url.path == link.url))
        ]
        [ text link.label ]


tabClasses : Bool -> List ( String, Bool )
tabClasses isCurrentUrl =
    [ ( "text-primary", isCurrentUrl )
    , ( "text-dark", not isCurrentUrl )
    ]
