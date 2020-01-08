module Quill exposing (Model, decoder, default, editor, encode, html)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Model =
    { html : String
    , text : String
    , delta : Value
    }


default : String -> Model
default str =
    { html = "<p>" ++ str ++ "<p>"
    , text = str
    , delta =
        ("{\"ops\": [{\"insert\":\"" ++ str ++ "\"}]}")
            |> Decode.decodeString Decode.value
            |> Result.toMaybe
            |> Maybe.withDefault Encode.null
    }


html : Model -> String
html model =
    model.html



-- Json


encode : Model -> Value
encode quill =
    Encode.object
        [ ( "html", Encode.string quill.html )
        , ( "text", Encode.string quill.text )
        , ( "delta", quill.delta )
        ]


decoder : Decoder Model
decoder =
    Decode.map3 Model
        (Decode.field "html" Decode.string)
        (Decode.field "text" Decode.string)
        (Decode.field "delta" Decode.value)



-- View


editor : (Model -> msg) -> Model -> Html msg
editor toMsg model =
    -- We need to wrap the quill element in a div because the toolbar seems to be
    -- added as a cousin of the current node, which messes up Elm's Html.Keyed stuff.
    div []
        [ node "websync-quill"
            [ attribute "data-html" model.html
            , attribute "data-delta" (Encode.encode 0 model.delta)
            , on "quill-change" (eventDecoder toMsg)
            ]
            []
        ]


eventDecoder : (Model -> msg) -> Decoder msg
eventDecoder toMsg =
    Decode.field "detail" (Decode.map toMsg decoder)
