module Renderer exposing (render)

import Content exposing (Content)
import Editor
import Html exposing (..)
import Html.Attributes exposing (..)
import Padding
import Row exposing (Row)


render : Editor.Email -> String
render email =
    let
        rows =
            email.rows
                |> List.map renderRow
                |> String.join "\n"
    in
    "<div class=\"ws-email theme-duolingo\">" ++ rows ++ "</div>"


renderRow : Row -> String
renderRow row =
    let
        stylingClass =
            case row.style of
                Row.Primary ->
                    "ws-row-primary"

                Row.PrimaryInverted ->
                    "ws-row-primary-inverted"

                Row.Secondary ->
                    "ws-row-secondary"

                Row.SecondaryInverted ->
                    "ws-row-secondary-inverted"
    in
    "<div class=\"ws-row "
        ++ stylingClass
        ++ "\" style=\""
        ++ Padding.render row.padding
        ++ "\">Here come dat boi</div>"
