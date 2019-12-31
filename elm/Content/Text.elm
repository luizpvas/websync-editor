module Content.Text exposing (QuillData, Text, default, mapQuillEvent, quill)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias QuillData =
    ( String, String, Value )


type alias Text =
    { html : String
    , text : String
    , delta : Value
    }


default : Text
default =
    { html = "<div>Lorem ipsum dolor sit amet, consectetur adipiscing elit.</div>"
    , text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
    , delta = Encode.null
    }


mapQuillEvent : QuillData -> Text -> Text
mapQuillEvent ( html, text, delta ) r =
    { r | html = html, text = text, delta = delta }


quill : (QuillData -> msg) -> Text -> Html msg
quill toMsg text =
    -- We need to wrap the quill element in a div because the toolbar seems to be
    -- added as a cousin of the current node, which messes up Elm's Html.Keyed stuff.
    div []
        [ node "websync-quill"
            [ attribute "data-html" text.html
            , attribute "data-delta" (Encode.encode 0 text.delta)
            , on "quill-change" (quillDecoder toMsg)
            ]
            []
        ]


quillDecoder : (QuillData -> msg) -> Decoder msg
quillDecoder toMsg =
    Decode.field "detail"
        (Decode.map3 (\a b c -> toMsg ( a, b, c ))
            (Decode.field "html" Decode.string)
            (Decode.field "text" Decode.string)
            (Decode.field "delta" Decode.value)
        )
