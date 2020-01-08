module Illustration exposing (Illustration, decoder, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)


type alias Illustration =
    { url : String
    , name : String
    }


decoder : Decoder Illustration
decoder =
    Decode.map2 Illustration
        (Decode.field "url" Decode.string)
        (Decode.field "name" Decode.string)


view : String -> Illustration -> msg -> Html msg
view color illustration click =
    node "websync-illustration"
        [ attribute "data-color" color
        , attribute "data-url" illustration.url
        , onClick click
        ]
        []
