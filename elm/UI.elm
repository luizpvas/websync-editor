module UI exposing (divider, editorHeader, editorSection, editorSectionInline)

import Html exposing (..)
import Html.Attributes exposing (..)


divider : Html msg
divider =
    div [ class "ws-divider" ] []


editorHeader : String -> List (Html msg) -> Html msg
editorHeader title buttons =
    div [ class "ws-editor-header" ]
        [ div [ class "ws-editor-header-text" ] [ text title ]
        , div [] buttons
        ]


editorSection : String -> List (Html msg) -> Html msg
editorSection title children =
    div [ class "ws-editor-section" ]
        [ div [ class "ws-editor-section-header" ] [ text title ]
        , div [] children
        ]


editorSectionInline : String -> List (Html msg) -> Html msg
editorSectionInline title rightSide =
    div [ class "ws-editor-section ws-editor-section--inline" ]
        [ div [ class "ws-editor-section-header" ] [ text title ]
        , div [] rightSide
        ]
