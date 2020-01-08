module Content.Text exposing (Text, decoder, default, editor, encode, mapPadding, mapQuill)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Icon
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Lang
import Padding exposing (Padding)
import Quill
import UI


type alias Text =
    { quill : Quill.Model
    , padding : Padding
    }


default : Text
default =
    { quill = Quill.default "Lorem ipsum..."
    , padding = Padding.default 0
    }



-- Json


encode : Text -> Value
encode text =
    Encode.object
        [ ( "quill", Quill.encode text.quill )
        , ( "padding", Padding.encode text.padding )
        ]


decoder : Decoder Text
decoder =
    Decode.map2 Text
        (Decode.field "quill" Quill.decoder)
        (Decode.field "padding" Padding.decoder)



-- Mapping


mapQuill : Quill.Model -> Text -> Text
mapQuill quill text =
    { text | quill = quill }


mapPadding : Padding.Msg -> Text -> Text
mapPadding msg text =
    { text | padding = Padding.map msg text.padding }



-- View


type alias ViewConfig msg =
    { text : Text
    , remove : msg
    , close : msg
    , setPadding : Padding.Msg -> msg
    }


editor : ViewConfig msg -> Html msg
editor view =
    div []
        [ UI.editorHeader Lang.textEditor
            [ button [ onClick view.remove ] [ Icon.trash ]
            , button [ onClick view.close ] [ Icon.close ]
            ]
        , Padding.editor { padding = view.text.padding, onInput = view.setPadding }
        ]
