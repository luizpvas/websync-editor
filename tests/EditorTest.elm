module EditorTest exposing (suite)

import BlockId
import Content exposing (Content)
import Content.Button as Button
import ContentId
import Editor
import Expect
import Padding
import Row exposing (Row)
import RowId
import Test exposing (..)


suite : Test
suite =
    describe "Editor spec"
        [ describe "Adding and removing rows"
            [ test "adds a row before another row" <|
                \_ ->
                    Editor.initWithRows [ dummyRow1, dummyRow2 ]
                        |> Editor.addRowBeforeAnotherRow dummyRow3 dummyRow2.id
                        |> Expect.equal { rows = [ dummyRow1, dummyRow3, dummyRow2 ] }
            , test "adds a row after another row" <|
                \_ ->
                    Editor.initWithRows [ dummyRow1, dummyRow2 ]
                        |> Editor.addRowAfterAnotherRow dummyRow3 dummyRow1.id
                        |> Expect.equal { rows = [ dummyRow1, dummyRow3, dummyRow2 ] }
            , test "adds a row before another row as the first" <|
                \_ ->
                    Editor.initWithRows [ dummyRow1, dummyRow2 ]
                        |> Editor.addRowBeforeAnotherRow dummyRow3 dummyRow1.id
                        |> Expect.equal { rows = [ dummyRow3, dummyRow1, dummyRow2 ] }
            , test "adds a row after another row as the last" <|
                \_ ->
                    Editor.initWithRows [ dummyRow1, dummyRow2 ]
                        |> Editor.addRowAfterAnotherRow dummyRow3 dummyRow2.id
                        |> Expect.equal { rows = [ dummyRow1, dummyRow2, dummyRow3 ] }
            ]
        , describe "Adding and removing content"
            [ test "adds content before another content" <|
                \_ ->
                    emailWithContents [ dummyContent 1, dummyContent 2 ]
                        |> Editor.addContentBeforeAnotherContent (dummyContent 3) (ContentId.fromInt 2)
                        |> Expect.equal (emailWithContents [ dummyContent 1, dummyContent 3, dummyContent 2 ])
            , test "adds content after another content" <|
                \_ ->
                    emailWithContents [ dummyContent 1, dummyContent 2 ]
                        |> Editor.addContentAfterAnotherContent (dummyContent 3) (ContentId.fromInt 1)
                        |> Expect.equal (emailWithContents [ dummyContent 1, dummyContent 3, dummyContent 2 ])
            , test "adds content before another content as the first" <|
                \_ ->
                    emailWithContents [ dummyContent 1, dummyContent 2 ]
                        |> Editor.addContentBeforeAnotherContent (dummyContent 3) (ContentId.fromInt 1)
                        |> Expect.equal (emailWithContents [ dummyContent 3, dummyContent 1, dummyContent 2 ])
            , test "adds content after another content as the last" <|
                \_ ->
                    emailWithContents [ dummyContent 1, dummyContent 2 ]
                        |> Editor.addContentAfterAnotherContent (dummyContent 3) (ContentId.fromInt 2)
                        |> Expect.equal (emailWithContents [ dummyContent 1, dummyContent 2, dummyContent 3 ])
            ]
        ]


emailWithContents : List Content -> Editor.Email
emailWithContents contents =
    Editor.initWithRows
        [ { id = RowId.fromInt 1
          , style = Row.Primary
          , fade = Row.Wave
          , padding = Padding.default 0
          , layout = Row.Row100 { id = BlockId.fromInt 1, width = 1.0, contents = [] }
          }
        ]


dummyContent : Int -> Content
dummyContent id =
    Content.Button (ContentId.fromInt id) Button.default


dummyRow1 : Row
dummyRow1 =
    Row.dummy (RowId.fromInt 1) (BlockId.fromInt 1) []


dummyRow2 : Row
dummyRow2 =
    Row.dummy (RowId.fromInt 2) (BlockId.fromInt 2) []


dummyRow3 : Row
dummyRow3 =
    Row.dummy (RowId.fromInt 3) (BlockId.fromInt 3) []
