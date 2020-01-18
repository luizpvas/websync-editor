module Content.Button exposing
    ( Button
    , decoder
    , default
    , editor
    , encode
    , mapPadding
    , mapTrix
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
import Trix
import UI


type alias Button =
    { trix : Trix.Model
    , url : String
    , padding : Padding
    }


default : Button
default =
    { trix = Trix.default Lang.clickMe
    , url = ""
    , padding = Padding.default 5
    }


mapTrix : Trix.Model -> Button -> Button
mapTrix trix button =
    { button | trix = trix }


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
        [ ( "trix", Trix.encode button.trix )
        , ( "url", Encode.string button.url )
        , ( "padding", Padding.encode button.padding )
        ]


decoder : Decoder Button
decoder =
    Decode.map3 Button
        (Decode.field "trix" Trix.decoder)
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
