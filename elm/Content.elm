module Content exposing (Content(..), ContentList, contentFromString, contentId, decoder, encode)

import Content.Button as Button
import Content.Divider as Divider
import Content.Image as Image
import Content.Text as Text
import ContentId exposing (ContentId)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias ContentList =
    List Content


type Content
    = Button ContentId Button.Button
    | Divider ContentId Divider.Divider
    | Image ContentId Image.Image
    | Text ContentId Text.Text
    | YoutubeVideo ContentId


contentId : Content -> ContentId
contentId content =
    case content of
        Button id _ ->
            id

        Divider id _ ->
            id

        Image id _ ->
            id

        Text id _ ->
            id

        YoutubeVideo id ->
            id


contentFromString : String -> Int -> Maybe ( Int, Content )
contentFromString name id =
    case name of
        "Button" ->
            Just ( id + 1, Button (ContentId.fromInt id) Button.default )

        "Divider" ->
            Just ( id + 1, Divider (ContentId.fromInt id) Divider.default )

        "Image" ->
            Just ( id + 1, Image (ContentId.fromInt id) Image.default )

        "Text" ->
            Just ( id + 1, Text (ContentId.fromInt id) Text.default )

        "YoutubeVideo" ->
            Just ( id + 1, YoutubeVideo (ContentId.fromInt id) )

        _ ->
            Nothing



-- Json


encode : Content -> Value
encode content =
    case content of
        Button id button ->
            Encode.object
                [ ( "type", Encode.string "Button" )
                , ( "id", ContentId.encode id )
                , ( "button", Button.encode button )
                ]

        Divider id divider ->
            Encode.object
                [ ( "type", Encode.string "Divider" )
                , ( "id", ContentId.encode id )
                , ( "divider", Divider.encode divider )
                ]

        Image id image ->
            Encode.object
                [ ( "type", Encode.string "Image" )
                , ( "id", ContentId.encode id )
                , ( "image", Image.encode image )
                ]

        Text id text ->
            Encode.object
                [ ( "type", Encode.string "Text" )
                , ( "id", ContentId.encode id )
                , ( "text", Text.encode text )
                ]

        YoutubeVideo id ->
            Debug.todo "implement this later"


decoder : Decoder Content
decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Button" ->
                        Decode.map2 Button
                            (Decode.field "id" ContentId.decoder)
                            (Decode.field "button" Button.decoder)

                    "Divider" ->
                        Decode.map2 Divider
                            (Decode.field "id" ContentId.decoder)
                            (Decode.field "divider" Divider.decoder)

                    "Image" ->
                        Decode.map2 Image
                            (Decode.field "id" ContentId.decoder)
                            (Decode.field "image" Image.decoder)

                    "Text" ->
                        Decode.map2 Text
                            (Decode.field "id" ContentId.decoder)
                            (Decode.field "text" Text.decoder)

                    _ ->
                        Decode.fail ("unrecognized content type: " ++ type_)
            )
