module Content.Image exposing
    ( Alignment
    , Image
    , ViewConfig
    , Width
    , default
    , editor
    , mapActionUrl
    , mapAlignment
    , mapAltDescription
    , mapPaddingAll
    , mapPaddingBottom
    , mapPaddingLeft
    , mapPaddingRight
    , mapPaddingTop
    , mapUploadProgress
    , mapUrl
    , mapWidth
    )

import ContentId exposing (ContentId)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Icon
import Lang
import Padding exposing (Padding)
import UI


type alias Image =
    { url : String
    , uploadProgress : Maybe Int
    , width : Width
    , alignment : Alignment
    , altDescription : String
    , actionUrl : Maybe String
    , padding : Padding
    }


type Width
    = Auto
    | Full
    | InPixels Int


type Alignment
    = Left
    | Center
    | Right


default : Image
default =
    { url = ""
    , uploadProgress = Nothing
    , width = Auto
    , alignment = Center
    , altDescription = ""
    , actionUrl = Nothing
    , padding = Padding.default
    }



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


mapPaddingAll : Int -> Image -> Image
mapPaddingAll padding image =
    { image | padding = Padding.all padding }


mapPaddingTop : Int -> Image -> Image
mapPaddingTop top image =
    { image | padding = Padding.mapTop top image.padding }


mapPaddingRight : Int -> Image -> Image
mapPaddingRight right image =
    { image | padding = Padding.mapRight right image.padding }


mapPaddingBottom : Int -> Image -> Image
mapPaddingBottom bottom image =
    { image | padding = Padding.mapBottom bottom image.padding }


mapPaddingLeft : Int -> Image -> Image
mapPaddingLeft left image =
    { image | padding = Padding.mapLeft left image.padding }



-- Views


type alias ViewConfig msg =
    { image : Image
    , contentId : ContentId
    , remove : msg
    , close : msg
    , setUrl : String -> msg
    , setWidth : Width -> msg
    , setAlignment : Alignment -> msg
    , setAltDescription : String -> msg
    , setActionUrl : String -> msg
    , setPaddingTop : Int -> msg
    , setPaddingRight : Int -> msg
    , setPaddingBottom : Int -> msg
    , setPaddingLeft : Int -> msg
    , setPaddingAll : Int -> msg
    }


editor : ViewConfig msg -> Html msg
editor view =
    div []
        [ UI.editorHeader Lang.imageEditor
            [ button [ onClick view.remove ] [ Icon.trash ]
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
            [ select []
                [ option [] [ text Lang.auto ]
                , option [] [ text Lang.fullWidth ]
                , option [] [ text Lang.widthInPixels ]
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
                , setTop = view.setPaddingTop
                , setRight = view.setPaddingRight
                , setBottom = view.setPaddingBottom
                , setLeft = view.setPaddingLeft
                , setAll = view.setPaddingAll
                }
            ]
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
            div [] [ text "Uploading..." ]


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
