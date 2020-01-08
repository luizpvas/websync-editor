module Padding exposing (Msg, Padding, attributes, decoder, default, editor, editorTopAndBottom, encode, map, render)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import UI


type alias Padding =
    { all : Int
    , top : Int
    , right : Int
    , bottom : Int
    , left : Int
    }


type Msg
    = All Int
    | Top Int
    | Right Int
    | TopAndBottom Int
    | Bottom Int
    | Left Int


default : Int -> Padding
default all =
    { all = all
    , top = all
    , right = all
    , bottom = all
    , left = all
    }


map : Msg -> Padding -> Padding
map msg padding =
    case msg of
        All all ->
            { all = all
            , top = all
            , right = all
            , bottom = all
            , left = all
            }

        Top top ->
            { padding | top = top }

        Right right ->
            { padding | right = right }

        Bottom bottom ->
            { padding | bottom = bottom }

        Left left ->
            { padding | left = left }

        TopAndBottom val ->
            { padding | all = val, top = val, bottom = val }



-- Rendering


attributes : Padding -> List (Attribute msg)
attributes padding =
    let
        attr =
            \attrName val ->
                if val == 0 then
                    Nothing

                else
                    Just <| style attrName (String.fromInt val ++ "px")
    in
    [ attr "padding-top" padding.top
    , attr "padding-right" padding.right
    , attr "padding-bottom" padding.bottom
    , attr "padding-left" padding.left
    ]
        |> List.filterMap identity


render : Padding -> String
render padding =
    let
        attr =
            \attrName val ->
                if val == 0 then
                    Nothing

                else
                    Just <| attrName ++ ": " ++ String.fromInt val ++ "px;"
    in
    [ attr "padding-top" padding.top
    , attr "padding-right" padding.right
    , attr "padding-bottom" padding.bottom
    , attr "padding-left" padding.left
    ]
        |> List.filterMap identity
        |> String.join ""



-- Views


type alias ViewConfig msg =
    { padding : Padding
    , onInput : Msg -> msg
    }


editor : ViewConfig msg -> Html msg
editor view =
    div []
        [ input
            [ type_ "range"
            , Html.Attributes.min "0"
            , Html.Attributes.max "50"
            , value (String.fromInt view.padding.all)
            , onIntInput All view.onInput
            ]
            []
        , UI.divider
        , input [ value (String.fromInt view.padding.top), onIntInput Top view.onInput ] []
        , input [ value (String.fromInt view.padding.right), onIntInput Right view.onInput ] []
        , input [ value (String.fromInt view.padding.bottom), onIntInput Bottom view.onInput ] []
        , input [ value (String.fromInt view.padding.left), onIntInput Left view.onInput ] []
        ]


editorTopAndBottom : ViewConfig msg -> Html msg
editorTopAndBottom view =
    div []
        [ input
            [ type_ "range"
            , Html.Attributes.min "0"
            , Html.Attributes.max "50"
            , value (String.fromInt view.padding.all)
            , onIntInput TopAndBottom view.onInput
            ]
            []
        , UI.divider
        , input [ value (String.fromInt view.padding.top), onIntInput Top view.onInput ] []
        , input [ value (String.fromInt view.padding.bottom), onIntInput Bottom view.onInput ] []
        ]


onIntInput : (Int -> Msg) -> (Msg -> msg) -> Attribute msg
onIntInput msg toMsg =
    onInput
        (\str ->
            case String.toInt str of
                Just num ->
                    toMsg (msg num)

                Nothing ->
                    toMsg (msg 0)
        )



-- Json


encode : Padding -> Value
encode padding =
    Encode.object
        [ ( "all", Encode.int padding.all )
        , ( "top", Encode.int padding.top )
        , ( "right", Encode.int padding.right )
        , ( "bottom", Encode.int padding.bottom )
        , ( "left", Encode.int padding.left )
        ]


decoder : Decoder Padding
decoder =
    Decode.map5 Padding
        (Decode.field "all" Decode.int)
        (Decode.field "top" Decode.int)
        (Decode.field "right" Decode.int)
        (Decode.field "bottom" Decode.int)
        (Decode.field "left" Decode.int)
