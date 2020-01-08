module EncodeDividerTest exposing (suite)

import Content.Divider as Divider
import Expect
import Json.Decode as Decode
import Padding
import Test exposing (..)


suite : Test
suite =
    describe "Divider"
        [ describe "stroke"
            [ testStroke Divider.Solid
            , testStroke Divider.Dashed
            , testStroke Divider.Dotted
            ]
        ]


testStroke : Divider.Stroke -> Test.Test
testStroke stroke =
    test ("divider stroke encoding: " ++ Divider.strokeToString stroke) <|
        \_ ->
            let
                encoded =
                    Divider.encode (dummyDividerWithStroke stroke)

                decoded =
                    Decode.decodeValue Divider.decoder encoded
            in
            Expect.equal (Ok (dummyDividerWithStroke stroke)) decoded


dummyDividerWithStroke : Divider.Stroke -> Divider.Divider
dummyDividerWithStroke stroke =
    { dummyDivider | stroke = stroke }


dummyDivider : Divider.Divider
dummyDivider =
    { widthPercentage = 100
    , stroke = Divider.Solid
    , thickness = 3
    , color = "#000"
    , padding = Padding.default 5
    }
