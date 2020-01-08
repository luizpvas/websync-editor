module EncodeImageTest exposing (suite)

import Content.Image as Image
import Expect
import Json.Decode as Decode
import Padding
import Test exposing (..)


suite : Test
suite =
    describe "Image"
        [ describe "encode/decode width"
            [ testWidth Image.Auto
            , testWidth Image.Full
            , testWidth (Image.InPixels 10)
            ]
        , describe "encode/decode alignment"
            [ testAlignment Image.Left
            , testAlignment Image.Center
            , testAlignment Image.Right
            ]
        ]


testWidth : Image.Width -> Test.Test
testWidth width =
    test ("image width encoding: " ++ Image.widthToString width) <|
        \_ ->
            let
                encoded =
                    Image.encode (dummyImageWithWidth width)

                decoded =
                    Decode.decodeValue Image.decoder encoded
            in
            Expect.equal (Ok (dummyImageWithWidth width)) decoded


testAlignment : Image.Alignment -> Test.Test
testAlignment alignment =
    test ("image alignment encoding: " ++ Image.alignmentToString alignment) <|
        \_ ->
            let
                encoded =
                    Image.encode (dummyImageWithAlignment alignment)

                decoded =
                    Decode.decodeValue Image.decoder encoded
            in
            Expect.equal (Ok (dummyImageWithAlignment alignment)) decoded


dummyImageWithWidth : Image.Width -> Image.Image
dummyImageWithWidth width =
    { dummyImage | width = width }


dummyImageWithAlignment : Image.Alignment -> Image.Image
dummyImageWithAlignment alignment =
    { dummyImage | alignment = alignment }


dummyImage : Image.Image
dummyImage =
    { pickingFromIllustrations = Nothing -- Not serialized
    , url = "my-site.com/img.png"
    , uploadProgress = Nothing -- Not serialized
    , width = Image.InPixels 10
    , alignment = Image.Center
    , altDescription = "A tree in the park"
    , actionUrl = Just "my-site.com"
    , padding = Padding.default 5
    }
