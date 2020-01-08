module Content.Button exposing
    ( Button
    , decoder
    , default
    , editor
    , encode
    , mapPadding
    , mapQuill
    , mapUrl
    )

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


type alias Button =
    { quill : Quill.Model
    , url : String
    , padding : Padding
    }


default : Button
default =
    { quill = Quill.default Lang.clickMe
    , url = ""
    , padding = Padding.default 5
    }


mapQuill : Quill.Model -> Button -> Button
mapQuill quill button =
    { button | quill = quill }


mapUrl : String -> Button -> Button
mapUrl url button =
    { button | url = url }


mapPadding : Padding.Msg -> Button -> Button
mapPadding msg button =
    { button | padding = Padding.map msg button.padding }



-- Json


encode : Button -> Value
encode button =
    Encode.object
        [ ( "quill", Quill.encode button.quill )
        , ( "url", Encode.string button.url )
        , ( "padding", Padding.encode button.padding )
        ]


decoder : Decoder Button
decoder =
    Decode.map3 Button
        (Decode.field "quill" Quill.decoder)
        (Decode.field "url" Decode.string)
        (Decode.field "padding" Padding.decoder)



-- View


type alias ViewConfig msg =
    { button : Button
    , remove : msg
    , close : msg
    , setUrl : String -> msg
    , setPadding : Padding.Msg -> msg
    }


editor : ViewConfig msg -> Html msg
editor view =
    div []
        [ UI.editorHeader Lang.buttonEditor
            [ button [ onClick view.remove ] [ Icon.trash ]
            , button [ onClick view.close ] [ Icon.close ]
            ]
        , UI.editorSectionInline Lang.url
            [ input [ value view.button.url, onInput view.setUrl, placeholder "https://..." ] []
            ]
        , Padding.editor { padding = view.button.padding, onInput = view.setPadding }
        ]
