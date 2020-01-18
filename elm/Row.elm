module Row exposing
    ( Alignment(..)
    , Row
    , RowLayout(..)
    , alignmentAttribute
    , backgroundAttribute
    , decoder
    , dummy
    , editor
    , encode
    , mapAlignment
    , mapBackground
    , mapPadding
    , rowFromString
    )

import Block exposing (Block)
import BlockId exposing (BlockId)
import Colorpicker
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
    , background : String
    , alignment : Alignment
    , padding : Padding
    }


type RowLayout
    = Row100 Block
    | Row50x50 Block Block
    | Row33x33x33 Block Block Block


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


alignmentAttribute : Row -> Attribute msg
alignmentAttribute row =
    case row.alignment of
        Start ->
            style "align-items" "flex-start"

        Center ->
            style "align-items" "center"

        End ->
            style "align-items" "flex-end"


backgroundAttribute : Row -> Attribute msg
backgroundAttribute row =
    style "background" row.background


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
                  , background = Colorpicker.white
                  , padding = Padding.default 0
                  , alignment = Start
                  , layout = Row100 |> newBlock (latestId + 1) 1.0
                  }
                )

        "Row50x50" ->
            Just
                ( latestId + 3
                , { id = RowId.fromInt latestId
                  , background = Colorpicker.white
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
                  , background = Colorpicker.white
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
    , background = Colorpicker.white
    , alignment = Start
    , padding = Padding.default 0
    }



-- Mapping


mapBackground : String -> Row -> Row
mapBackground background row =
    { row | background = background }


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
    , setBackground : String -> msg
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
        , UI.editorSectionInline Lang.color
            [ Colorpicker.view view.row.background view.setBackground
            ]
        , UI.editorSectionInline Lang.alignment
            [ viewAlignmentOptions view
                [ ( Start, Icon.alignLeft )
                , ( Center, Icon.alignCenter )
                , ( End, Icon.alignRight )
                ]
            ]
        , Padding.editorTopAndBottom { padding = view.row.padding, onInput = view.setPadding }
        ]


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
        , ( "background", Colorpicker.encode row.background )
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
    Decode.map5 Row
        (Decode.field "id" RowId.decoder)
        (Decode.field "layout" layoutDecoder)
        (Decode.field "background" Colorpicker.decoder)
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
