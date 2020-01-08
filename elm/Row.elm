module Row exposing
    ( Alignment(..)
    , Fade(..)
    , Row
    , RowLayout(..)
    , RowStyle(..)
    , alignmentAttribute
    , decoder
    , dummy
    , editor
    , encode
    , mapAlignment
    , mapFade
    , mapPadding
    , mapStyle
    , rowFromString
    , rowStyleClass
    )

import Block exposing (Block)
import BlockId exposing (BlockId)
import Content exposing (ContentList)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Icon
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Lang
import Padding exposing (Padding)
import RowId exposing (RowId)
import UI


type alias Row =
    { id : RowId
    , layout : RowLayout
    , style : RowStyle
    , fade : Fade
    , alignment : Alignment
    , padding : Padding
    }


type RowLayout
    = Row100 Block
    | Row50x50 Block Block
    | Row33x33x33 Block Block Block


type RowStyle
    = Primary
    | Secondary
    | PrimaryInverted
    | SecondaryInverted


styleToString : RowStyle -> String
styleToString style =
    case style of
        Primary ->
            "Primary"

        Secondary ->
            "Secondary"

        PrimaryInverted ->
            "PrimaryInverted"

        SecondaryInverted ->
            "SecondaryInverted"


styleFromString : String -> RowStyle
styleFromString str =
    case str of
        "Primary" ->
            Primary

        "Secondary" ->
            Secondary

        "PrimaryInverted" ->
            PrimaryInverted

        "SecondaryInverted" ->
            SecondaryInverted

        _ ->
            Primary


type Fade
    = HardCap
    | Wave
    | TiltLeft
    | TiltRight


fadeToString : Fade -> String
fadeToString fade =
    case fade of
        HardCap ->
            "HardCap"

        Wave ->
            "Wave"

        TiltLeft ->
            "TiltLeft"

        TiltRight ->
            "TiltRight"


fadeFromString : String -> Fade
fadeFromString str =
    case str of
        "HardCap" ->
            HardCap

        "Wave" ->
            Wave

        "TiltLeft" ->
            TiltLeft

        "TiltRight" ->
            TiltRight

        _ ->
            HardCap


type Alignment
    = Start
    | Center
    | End


alignmentToString : Alignment -> String
alignmentToString alignment =
    case alignment of
        Start ->
            "Start"

        Center ->
            "Center"

        End ->
            "End"


alignmentFromString : String -> Alignment
alignmentFromString str =
    case str of
        "Start" ->
            Start

        "Center" ->
            Center

        "End" ->
            End

        _ ->
            Start


rowStyleClass : Row -> String
rowStyleClass row =
    case row.style of
        Primary ->
            "ws-row-primary"

        Secondary ->
            "ws-row-secondary"

        PrimaryInverted ->
            "ws-row-primary-inverted"

        SecondaryInverted ->
            "ws-row-secondary-inverted"


alignmentAttribute : Row -> Attribute msg
alignmentAttribute row =
    case row.alignment of
        Start ->
            style "align-items" "flex-start"

        Center ->
            style "align-items" "center"

        End ->
            style "align-items" "flex-end"


rowFromString : String -> Int -> Maybe ( Int, Row )
rowFromString rowName latestId =
    let
        newBlock =
            \id width row ->
                row { id = BlockId.fromInt id, width = width, contents = [] }
    in
    case rowName of
        "Row100" ->
            Just
                ( latestId + 2
                , { id = RowId.fromInt latestId
                  , style = Primary
                  , fade = HardCap
                  , padding = Padding.default 0
                  , alignment = Start
                  , layout = Row100 |> newBlock (latestId + 1) 1.0
                  }
                )

        "Row50x50" ->
            Just
                ( latestId + 3
                , { id = RowId.fromInt latestId
                  , style = Primary
                  , fade = HardCap
                  , padding = Padding.default 0
                  , alignment = Start
                  , layout =
                        Row50x50
                            |> newBlock (latestId + 1) 0.5
                            |> newBlock (latestId + 2) 0.5
                  }
                )

        "Row33x33x33" ->
            Just
                ( latestId + 4
                , { id = RowId.fromInt latestId
                  , style = Primary
                  , fade = HardCap
                  , padding = Padding.default 0
                  , alignment = Start
                  , layout =
                        Row33x33x33
                            |> newBlock (latestId + 1) 0.3333
                            |> newBlock (latestId + 2) 0.3333
                            |> newBlock (latestId + 3) 0.3333
                  }
                )

        _ ->
            Nothing


dummy : RowId -> BlockId -> ContentList -> Row
dummy id blockId contents =
    { id = id
    , layout = Row100 { id = blockId, contents = contents, width = 1.0 }
    , style = Primary
    , fade = HardCap
    , alignment = Start
    , padding = Padding.default 0
    }



-- Mapping


mapStyle : RowStyle -> Row -> Row
mapStyle style row =
    { row | style = style }


mapFade : Fade -> Row -> Row
mapFade fade row =
    { row | fade = fade }


mapPadding : Padding.Msg -> Row -> Row
mapPadding msg row =
    { row | padding = Padding.map msg row.padding }


mapAlignment : Alignment -> Row -> Row
mapAlignment alignment row =
    { row | alignment = alignment }



-- View


type alias ViewConfig msg =
    { row : Row
    , close : msg
    , remove : msg
    , setStyle : RowStyle -> msg
    , setFade : Fade -> msg
    , setPadding : Padding.Msg -> msg
    , setAlignment : Alignment -> msg
    }


editor : ViewConfig msg -> Html msg
editor view =
    div []
        [ UI.editorHeader Lang.rowEditor
            [ button [ onClick view.remove ] [ Icon.trash ]
            , button [ onClick view.close ] [ Icon.close ]
            ]
        , UI.editorSectionInline Lang.styling
            [ viewStyling view
                [ ( Primary, Lang.primary )
                , ( Secondary, Lang.secondary )
                , ( PrimaryInverted, Lang.primaryInverted )
                , ( SecondaryInverted, Lang.secondaryInverted )
                ]
            ]
        , UI.editorSectionInline Lang.alignment
            [ viewAlignmentOptions view
                [ ( Start, Icon.alignLeft )
                , ( Center, Icon.alignCenter )
                , ( End, Icon.alignRight )
                ]
            ]
        , UI.editorSection Lang.rowFade
            [ viewFades view
                [ ( HardCap, Icon.fadeNone )
                , ( Wave, Icon.fadeWave )
                , ( TiltLeft, Icon.fadeTiltLeft )
                , ( TiltRight, Icon.fadeTiltRight )
                ]
            ]
        , Padding.editorTopAndBottom { padding = view.row.padding, onInput = view.setPadding }
        ]


viewStyling view options =
    select [ onInput (styleFromString >> view.setStyle) ]
        (List.map
            (\( val, txt ) ->
                option [ value (styleToString val), selected (view.row.style == val) ] [ text txt ]
            )
            options
        )


viewFades view options =
    div []
        (List.map
            (\( val, txt ) ->
                div [ class "ws-row-fade-option", onClick (view.setFade val) ] [ txt ]
            )
            options
        )


viewAlignmentOptions : ViewConfig msg -> List ( Alignment, Html msg ) -> Html msg
viewAlignmentOptions view options =
    div [ class "ws-button-group" ]
        (List.map
            (\( alignment, icon ) ->
                if view.row.alignment == alignment then
                    button [ class "ws-button ws-active", onClick (view.setAlignment alignment) ] [ icon ]

                else
                    button [ class "ws-button", onClick (view.setAlignment alignment) ] [ icon ]
            )
            options
        )



-- Json


encode : Row -> Value
encode row =
    Encode.object
        [ ( "id", RowId.encode row.id )
        , ( "layout", encodeLayout row.layout )
        , ( "style", styleToString row.style |> Encode.string )
        , ( "fade", fadeToString row.fade |> Encode.string )
        , ( "alignment", alignmentToString row.alignment |> Encode.string )
        , ( "padding", Padding.encode row.padding )
        ]


encodeLayout : RowLayout -> Value
encodeLayout layout =
    case layout of
        Row100 block ->
            Encode.object
                [ ( "type", Encode.string "Row100" )
                , ( "block", Block.encode block )
                ]

        Row50x50 left right ->
            Encode.object
                [ ( "type", Encode.string "Row50x50" )
                , ( "left", Block.encode left )
                , ( "right", Block.encode right )
                ]

        Row33x33x33 left middle right ->
            Encode.object
                [ ( "type", Encode.string "Row33x33x33" )
                , ( "left", Block.encode left )
                , ( "middle", Block.encode middle )
                , ( "right", Block.encode right )
                ]


decoder : Decoder Row
decoder =
    Decode.map6 Row
        (Decode.field "id" RowId.decoder)
        (Decode.field "layout" layoutDecoder)
        (Decode.field "style" (Decode.map styleFromString Decode.string))
        (Decode.field "fade" (Decode.map fadeFromString Decode.string))
        (Decode.field "alignment" (Decode.map alignmentFromString Decode.string))
        (Decode.field "padding" Padding.decoder)


layoutDecoder : Decoder RowLayout
layoutDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Row100" ->
                        Decode.map Row100
                            (Decode.field "block" Block.decoder)

                    "Row50x50" ->
                        Decode.map2 Row50x50
                            (Decode.field "left" Block.decoder)
                            (Decode.field "right" Block.decoder)

                    "Row33x33x33" ->
                        Decode.map3 Row33x33x33
                            (Decode.field "left" Block.decoder)
                            (Decode.field "middle" Block.decoder)
                            (Decode.field "right" Block.decoder)

                    _ ->
                        Decode.fail ("did not recognize row layout: " ++ type_)
            )
