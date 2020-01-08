module Content.Divider exposing
    ( Divider
    , Stroke(..)
    , decoder
    , default
    , editor
    , encode
    , mapColor
    , mapPadding
    , mapStroke
    , mapThickness
    , mapWidthPercentage
    , strokeToString
    )

import Colorpicker
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Icon
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Lang
import Padding exposing (Padding)
import UI


type alias Divider =
    { widthPercentage : Int
    , stroke : Stroke
    , thickness : Int
    , color : String
    , padding : Padding
    }


type Stroke
    = Solid
    | Dashed
    | Dotted


strokeToString : Stroke -> String
strokeToString stroke =
    case stroke of
        Solid ->
            "Solid"

        Dashed ->
            "Dashed"

        Dotted ->
            "Dotted"


strokeFromString : String -> Stroke
strokeFromString str =
    case str of
        "Solid" ->
            Solid

        "Dashed" ->
            Dashed

        "Dotted" ->
            Dotted

        _ ->
            Solid


mapWidthPercentage : Int -> Divider -> Divider
mapWidthPercentage widthPct divider =
    { divider | widthPercentage = widthPct }


mapStroke : Stroke -> Divider -> Divider
mapStroke stroke divider =
    { divider | stroke = stroke }


mapThickness : Int -> Divider -> Divider
mapThickness thickness divider =
    { divider | thickness = thickness }


mapColor : String -> Divider -> Divider
mapColor hex divider =
    { divider | color = hex }


mapPadding : Padding.Msg -> Divider -> Divider
mapPadding msg divider =
    { divider | padding = Padding.map msg divider.padding }


default : Divider
default =
    { widthPercentage = 100
    , stroke = Solid
    , thickness = 1
    , color = "#718096"
    , padding = Padding.default 5
    }



-- Json


encode : Divider -> Value
encode divider =
    Encode.object
        [ ( "width_percentage", Encode.int divider.widthPercentage )
        , ( "stroke", strokeToString divider.stroke |> Encode.string )
        , ( "thickness", Encode.int divider.thickness )
        , ( "color", Encode.string divider.color )
        , ( "padding", Padding.encode divider.padding )
        ]


decoder : Decoder Divider
decoder =
    Decode.map5 Divider
        (Decode.field "width_percentage" Decode.int)
        (Decode.field "stroke" (Decode.map strokeFromString Decode.string))
        (Decode.field "thickness" Decode.int)
        (Decode.field "color" Decode.string)
        (Decode.field "padding" Padding.decoder)



-- View


type alias ViewConfig msg =
    { divider : Divider
    , remove : msg
    , close : msg
    , setWidthPercentage : Int -> msg
    , setStroke : Stroke -> msg
    , setThickness : Int -> msg
    , setColor : String -> msg
    , setPadding : Padding.Msg -> msg
    }


editor : ViewConfig msg -> Html msg
editor view =
    div []
        [ UI.editorHeader Lang.dividerEditor
            [ button [ onClick view.remove ] [ Icon.trash ]
            , button [ onClick view.close ] [ Icon.close ]
            ]
        , UI.editorSectionInline Lang.width
            [ input
                [ type_ "range"
                , Html.Attributes.min "0"
                , Html.Attributes.max "100"
                , value (String.fromInt view.divider.widthPercentage)
                , onIntInput view.setWidthPercentage
                ]
                []
            ]
        , UI.editorSectionInline Lang.strokeStyle
            [ viewStrokeOptions view
                [ ( Solid, Lang.solid )
                , ( Dashed, Lang.dashed )
                , ( Dotted, Lang.dotted )
                ]
            ]
        , UI.editorSectionInline Lang.thickness
            [ input
                [ type_ "range"
                , Html.Attributes.min "0"
                , Html.Attributes.max "20"
                , value (String.fromInt view.divider.thickness)
                , onIntInput view.setThickness
                ]
                []
            ]
        , UI.editorSectionInline Lang.color
            [ Colorpicker.view view.divider.color view.setColor
            ]
        , Padding.editor
            { padding = view.divider.padding
            , onInput = view.setPadding
            }
        ]


viewStrokeOptions : ViewConfig msg -> List ( Stroke, String ) -> Html msg
viewStrokeOptions view options =
    select [ onInput (strokeFromString >> view.setStroke) ]
        (List.map
            (\( val, txt ) ->
                option [ value (strokeToString val), selected (view.divider.stroke == val) ] [ text txt ]
            )
            options
        )


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
