module Content.Image exposing
    ( Alignment(..)
    , Image
    , ViewConfig
    , Width(..)
    , alignmentToString
    , decoder
    , default
    , editor
    , encode
    , isEmpty
    , mapActionUrl
    , mapAlignment
    , mapAltDescription
    , mapCloseIllustrationPicker
    , mapIllustrationColor
    , mapPadding
    , mapPickIllustration
    , mapSearchIllustrations
    , mapTransitionToIllustrationPicker
    , mapUploadProgress
    , mapUrl
    , mapWidth
    , widthToString
    )

import Colorpicker
import ContentId exposing (ContentId)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Icon
import Illustration exposing (Illustration)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode exposing (Value)
import Lang
import Padding exposing (Padding)
import UI


type alias Image =
    { pickingFromIllustrations : Maybe PickingFromIllustrations
    , url : String
    , uploadProgress : Maybe Int
    , width : Width
    , alignment : Alignment
    , altDescription : String
    , actionUrl : Maybe String
    , padding : Padding
    }


type alias PickingFromIllustrations =
    { search : String
    , color : String
    }


type Width
    = Auto
    | Full
    | InPixels Int


widthToString : Width -> String
widthToString width =
    case width of
        Auto ->
            "Auto"

        Full ->
            "Full"

        InPixels _ ->
            "InPixels"


widthFromString : String -> Width
widthFromString str =
    case str of
        "Auto" ->
            Auto

        "Full" ->
            Full

        "InPixels" ->
            InPixels 50

        _ ->
            Auto


type Alignment
    = Left
    | Center
    | Right


alignmentToString : Alignment -> String
alignmentToString alignment =
    case alignment of
        Left ->
            "Left"

        Center ->
            "Center"

        Right ->
            "Right"


alignmentFromString : String -> Alignment
alignmentFromString str =
    case str of
        "Left" ->
            Left

        "Center" ->
            Center

        "Right" ->
            Right

        _ ->
            Left


default : Image
default =
    { pickingFromIllustrations = Nothing
    , url = ""
    , uploadProgress = Nothing
    , width = Full
    , alignment = Center
    , altDescription = ""
    , actionUrl = Nothing
    , padding = Padding.default 0
    }


isEmpty : Image -> Bool
isEmpty image =
    String.length image.url == 0



-- Updating


mapUploadProgress : Int -> Image -> Image
mapUploadProgress percentage image =
    { image | uploadProgress = Just percentage }


mapUrl : String -> Image -> Image
mapUrl url image =
    { image | url = url, uploadProgress = Nothing }


mapWidth : Width -> Image -> Image
mapWidth width image =
    { image | width = width }


mapAlignment : Alignment -> Image -> Image
mapAlignment alignment image =
    { image | alignment = alignment }


mapAltDescription : String -> Image -> Image
mapAltDescription altDescription image =
    { image | altDescription = altDescription }


mapActionUrl : String -> Image -> Image
mapActionUrl actionUrl image =
    if actionUrl == "" then
        { image | actionUrl = Nothing }

    else
        { image | actionUrl = Just actionUrl }


mapPadding : Padding.Msg -> Image -> Image
mapPadding msg image =
    { image | padding = Padding.map msg image.padding }


mapTransitionToIllustrationPicker : Image -> Image
mapTransitionToIllustrationPicker image =
    { image | pickingFromIllustrations = Just { search = "", color = "#6c63ff" } }


mapCloseIllustrationPicker : Image -> Image
mapCloseIllustrationPicker image =
    { image | pickingFromIllustrations = Nothing }


mapSearchIllustrations : String -> Image -> Image
mapSearchIllustrations search image =
    case image.pickingFromIllustrations of
        Just picking ->
            let
                mapped =
                    { picking | search = search }
            in
            { image | pickingFromIllustrations = Just mapped }

        Nothing ->
            image


mapIllustrationColor : String -> Image -> Image
mapIllustrationColor color image =
    case image.pickingFromIllustrations of
        Just picking ->
            let
                mapped =
                    { picking | color = color }
            in
            { image | pickingFromIllustrations = Just mapped }

        Nothing ->
            image


mapPickIllustration : String -> Image -> Image
mapPickIllustration base64 image =
    { image | url = base64, pickingFromIllustrations = Nothing }



-- Json


encode : Image -> Value
encode image =
    Encode.object
        [ ( "url", Encode.string image.url )
        , ( "width", encodeWidth image.width )
        , ( "alignment", alignmentToString image.alignment |> Encode.string )
        , ( "alt_description", Encode.string image.altDescription )
        , ( "action_url", Encode.string (Maybe.withDefault "" image.actionUrl) )
        , ( "padding", Padding.encode image.padding )
        ]


encodeWidth : Width -> Value
encodeWidth width =
    case width of
        Auto ->
            Encode.object [ ( "type", Encode.string "Auto" ) ]

        Full ->
            Encode.object [ ( "type", Encode.string "Full" ) ]

        InPixels pixels ->
            Encode.object
                [ ( "type", Encode.string "InPixels" )
                , ( "pixels", Encode.int pixels )
                ]


decoder : Decoder Image
decoder =
    Decode.succeed Image
        |> hardcoded Nothing
        |> Json.Decode.Pipeline.required "url" Decode.string
        |> hardcoded Nothing
        |> Json.Decode.Pipeline.required "width" widthDecoder
        |> Json.Decode.Pipeline.required "alignment" (Decode.map alignmentFromString Decode.string)
        |> optional "alt_description" Decode.string ""
        |> optional "action_url" (Decode.map Just Decode.string) Nothing
        |> Json.Decode.Pipeline.required "padding" Padding.decoder


widthDecoder : Decoder Width
widthDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Auto" ->
                        Decode.succeed Auto

                    "Full" ->
                        Decode.succeed Full

                    "InPixels" ->
                        Decode.map InPixels
                            (Decode.field "pixels" Decode.int)

                    _ ->
                        Decode.fail ("unrecognized width type: " ++ type_)
            )



-- Views


type alias ViewConfig msg =
    { image : Image
    , contentId : ContentId
    , illustrations : List Illustration
    , remove : msg
    , close : msg
    , setUrl : String -> msg
    , setWidth : Width -> msg
    , setAlignment : Alignment -> msg
    , setAltDescription : String -> msg
    , setActionUrl : String -> msg
    , setPadding : Padding.Msg -> msg
    , startPickingIllustrations : msg
    , stopPickingIllustrations : msg
    , setIllustrationsSearch : String -> msg
    , setIllustrationsColor : String -> msg
    , pickIllustration : String -> msg
    }


editor : ViewConfig msg -> Html msg
editor view =
    case view.image.pickingFromIllustrations of
        Just picking ->
            div []
                [ UI.editorHeader Lang.imageEditor
                    [ button [ onClick view.stopPickingIllustrations ] [ Icon.close ]
                    ]
                , input [ onInput view.setIllustrationsSearch, value picking.search, placeholder Lang.search ] []
                , Colorpicker.view picking.color view.setIllustrationsColor
                , div []
                    (List.map
                        (\illustration ->
                            node "websync-illustration"
                                [ attribute "data-url" illustration.url
                                , attribute "data-name" illustration.name
                                , attribute "data-color" picking.color
                                , on "illustration-click"
                                    (Decode.at [ "detail", "base64" ] Decode.string
                                        |> Decode.map view.pickIllustration
                                    )
                                ]
                                []
                        )
                        view.illustrations
                    )
                ]

        Nothing ->
            div []
                [ UI.editorHeader Lang.imageEditor
                    [ button [ onClick view.startPickingIllustrations ] [ Icon.illustration ]
                    , button [ onClick view.remove ] [ Icon.trash ]
                    , button [ onClick view.close ] [ Icon.close ]
                    ]
                , viewImagePicker view
                , UI.editorSection Lang.altDescription
                    [ text Lang.altDescriptionHelp
                    , input
                        [ class "ws-input"
                        , value view.image.altDescription
                        , onInput view.setAltDescription
                        , placeholder Lang.altDescriptionPlaceholder
                        ]
                        []
                    ]
                , UI.editorSectionInline Lang.width
                    [ viewWidthPicker view
                        [ ( widthToString Auto, Lang.auto )
                        , ( widthToString Full, Lang.fullWidth )
                        , ( widthToString (InPixels 0), Lang.widthInPixels )
                        ]
                    ]
                , UI.editorSectionInline Lang.alignment
                    [ viewAlignmentOptions view
                        [ ( Left, Icon.alignLeft )
                        , ( Center, Icon.alignCenter )
                        , ( Right, Icon.alignRight )
                        ]
                    ]
                , UI.editorSection Lang.clickAction
                    [ input [ value (Maybe.withDefault "" view.image.actionUrl), onInput view.setActionUrl ] []
                    ]
                , UI.editorSection Lang.padding
                    [ Padding.editor
                        { padding = view.image.padding
                        , onInput = view.setPadding
                        }
                    ]
                ]


viewWidthPicker : ViewConfig msg -> List ( String, String ) -> Html msg
viewWidthPicker view options =
    let
        additional =
            case view.image.width of
                Auto ->
                    text ""

                Full ->
                    text ""

                InPixels pixels ->
                    input
                        [ type_ "number"
                        , value (String.fromInt pixels)
                        , onInput (\str -> view.setWidth (InPixels (Maybe.withDefault 0 (String.toInt str))))
                        ]
                        []
    in
    div []
        [ select [ onInput (widthFromString >> view.setWidth) ]
            (List.map
                (\( val, label ) ->
                    option [ value val, selected (val == widthToString view.image.width) ] [ text label ]
                )
                options
            )
        , additional
        ]


viewImagePicker : ViewConfig msg -> Html msg
viewImagePicker view =
    case view.image.uploadProgress of
        Nothing ->
            div [ class "ws-p-10" ]
                [ node "websync-file-upload" [ attribute "data-content-id" (ContentId.toString view.contentId) ] []
                , text Lang.or
                , input [ placeholder Lang.pasteTheImageUrlHere, value view.image.url, onInput view.setUrl ] []
                ]

        Just percentage ->
            div [ class "ws-upload-progress" ]
                [ div
                    [ class "ws-upload-progress-uploaded"
                    , style "width" (String.fromInt percentage ++ "%")
                    ]
                    []
                ]


viewAlignmentOptions : ViewConfig msg -> List ( Alignment, Html msg ) -> Html msg
viewAlignmentOptions view options =
    div [ class "ws-button-group" ]
        (List.map
            (\( alignment, icon ) ->
                if view.image.alignment == alignment then
                    button [ class "ws-button ws-active", onClick (view.setAlignment alignment) ] [ icon ]

                else
                    button [ class "ws-button", onClick (view.setAlignment alignment) ] [ icon ]
            )
            options
        )
