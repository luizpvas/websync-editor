module EncodeTextTest exposing (suite)

import Content.Text as Text
import Expect
import Json.Decode as Decode
import Quill
import Test exposing (..)


suite : Test
suite =
    describe "Text"
        [ test "encode/decode from JSON" <|
            \_ ->
                let
                    encoded =
                        Text.encode dummyText

                    decoded =
                        Decode.decodeValue Text.decoder encoded
                in
                Expect.equal (Ok dummyText) decoded
        ]


dummyText : Text.Text
dummyText =
    { quill = Quill.default
    }
