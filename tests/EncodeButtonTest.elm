module EncodeButtonTest exposing (suite)

import Content.Button as Button
import Expect
import Json.Decode as Decode
import Padding
import Quill
import Test exposing (..)


suite : Test
suite =
    describe "Button"
        [ test "encode/decode from JSON" <|
            \_ ->
                let
                    encoded =
                        Button.encode dummyButton

                    decoded =
                        Decode.decodeValue Button.decoder encoded
                in
                Expect.equal (Ok dummyButton) decoded
        ]


dummyButton : Button.Button
dummyButton =
    { quill = Quill.default
    , url = "teclia.com"
    , padding = Padding.default 5
    }
