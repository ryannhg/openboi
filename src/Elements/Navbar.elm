module Elements.Navbar exposing (view)

import Context
import Html exposing (..)
import Html.Attributes exposing (..)
import Url exposing (Url)
import Elements.Navigation exposing (Link, links)


view : Context.Model -> Html msg
view context =
    div
        [ class "pad-sm hide-md-up"
        , style "box-shadow" "0 0 16px rgba(0, 0, 0, .10)"
        ]
        [ div [ class "row", style "align-items" "center" ]
            [ h3 [ class "col mar-0" ]
                [ viewLink context.url (Link "/" "OpenBoi")
                ]
            , div [ class "col-auto hide-xs" ]
                [ div [ class "row row-flush" ] (List.map (viewLink context.url) links) ]
            ]
        ]


viewLink : Url -> Link -> Html msg
viewLink url link =
    a
        [ href link.url
        , class "col-auto"
        , classList (linkClasses (url.path == link.url))
        ]
        [ text link.label ]


linkClasses : Bool -> List ( String, Bool )
linkClasses isCurrentUrl =
    [ ( "text-primary", isCurrentUrl )
    , ( "text-dark", not isCurrentUrl )
    ]
