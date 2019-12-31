module Error exposing (Error(..), view)

import Browser.Dom
import Html exposing (..)


type Error
    = AllGood
    | StringError String
    | DomError String Browser.Dom.Error


view : Error -> Html msg
view err =
    case err of
        AllGood ->
            text ""

        StringError description ->
            div [] [ text description ]

        DomError description domErr ->
            case domErr of
                Browser.Dom.NotFound id ->
                    div [] [ text (description ++ id) ]
