module RawHtml exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)


view : String -> Html msg
view html =
    node "websync-html" [ attribute "data-html" html ] []
