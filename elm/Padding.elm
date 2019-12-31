module Padding exposing (Padding, all, default, editor, mapBottom, mapLeft, mapRight, mapTop)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import UI


type alias Padding =
    { all : Int
    , top : Int
    , right : Int
    , bottom : Int
    , left : Int
    }


default : Padding
default =
    { all = 0
    , top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


all : Int -> Padding
all padding =
    { all = padding
    , top = padding
    , right = padding
    , bottom = padding
    , left = padding
    }


mapTop : Int -> Padding -> Padding
mapTop top padding =
    { padding | top = top }


mapRight : Int -> Padding -> Padding
mapRight right padding =
    { padding | right = right }


mapBottom : Int -> Padding -> Padding
mapBottom bottom padding =
    { padding | bottom = bottom }


mapLeft : Int -> Padding -> Padding
mapLeft left padding =
    { padding | left = left }


type alias ViewConfig msg =
    { padding : Padding
    , setAll : Int -> msg
    , setTop : Int -> msg
    , setRight : Int -> msg
    , setBottom : Int -> msg
    , setLeft : Int -> msg
    }


editor : ViewConfig msg -> Html msg
editor view =
    div []
        [ input [ type_ "range", Html.Attributes.min "0", Html.Attributes.max "50", value (String.fromInt view.padding.all), onIntInput view.setAll ] []
        , UI.divider
        , input [ value (String.fromInt view.padding.top), onIntInput view.setTop ] []
        , input [ value (String.fromInt view.padding.right), onIntInput view.setRight ] []
        , input [ value (String.fromInt view.padding.bottom), onIntInput view.setBottom ] []
        , input [ value (String.fromInt view.padding.left), onIntInput view.setLeft ] []
        ]


onIntInput : (Int -> msg) -> Attribute msg
onIntInput toMsg =
    onInput
        (\str ->
            case String.toInt str of
                Just num ->
                    toMsg num

                Nothing ->
                    toMsg 0
        )
