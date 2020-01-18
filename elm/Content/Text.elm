module Content.Text exposing (Text, decoder, default, editor, encode, mapPadding, mapTrix)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Icon
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Lang
import Padding exposing (Padding)
import Trix
import UI


type alias Text =
    { trix : Trix.Model
    , padding : Padding
    }


default : Text
default =
    { trix = Trix.default "Lorem ipsum..."
    , padding = Padding.default 0
    }



-- Json


encode : Text -> Value
encode text =
    Encode.object
        [ ( "trix", Trix.encode text.trix )
        , ( "padding", Padding.encode text.padding )
        ]


decoder : Decoder Text
decoder =
    Decode.map2 Text
        (Decode.field "trix" Trix.decoder)
        (Decode.field "padding" Padding.decoder)



-- Mapping


mapTrix : Trix.Model -> Text -> Text
mapTrix trix text =
    { text | trix = trix }


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
