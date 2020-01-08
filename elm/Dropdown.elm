module Dropdown exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)


view : Html msg -> Html msg -> Html msg
view trigger dropdown =
    node "websync-dropdown"
        []
        [ div [ attribute "data-trigger" "" ] [ trigger ]
        , div [ attribute "data-dropdown" "", class "ws-dropdown" ] [ dropdown ]
        ]
