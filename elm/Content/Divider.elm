module Content.Divider exposing (Divider, Stroke, default, editor, mapColor, mapStroke, mapThickness, mapWidthPercentage)

import Colorpicker
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Icon
import Lang
import UI


type alias Divider =
    { widthPercentage : Int
    , stroke : Stroke
    , thickness : Int
    , color : String
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


default : Divider
default =
    { widthPercentage = 100
    , stroke = Solid
    , thickness = 1
    , color = "#718096"
    }



-- View


type alias ViewConfig msg =
    { divider : Divider
    , remove : msg
    , close : msg
    , setWidthPercentage : Int -> msg
    , setStroke : Stroke -> msg
    , setThickness : Int -> msg
    , setColor : String -> msg
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
