module EditorTest exposing (suite)

import BlockId
import ContentId
import Editor
import Expect
import RowId
import Test exposing (..)


suite : Test
suite =
    describe "Editor spec"
        [ describe "Adding and removing rows"
            [ test "adds a row before another row" <|
                \_ ->
                    Editor.initWithRows [ dummyRow1, dummyRow2 ]
                        |> Editor.addRowBeforeAnotherRow dummyRow3 (Editor.rowId dummyRow2)
                        |> Expect.equal { rows = [ dummyRow1, dummyRow3, dummyRow2 ] }
            , test "adds a row after another row" <|
                \_ ->
                    Editor.initWithRows [ dummyRow1, dummyRow2 ]
                        |> Editor.addRowAfterAnotherRow dummyRow3 (Editor.rowId dummyRow1)
                        |> Expect.equal { rows = [ dummyRow1, dummyRow3, dummyRow2 ] }
            , test "adds a row before another row as the first" <|
                \_ ->
                    Editor.initWithRows [ dummyRow1, dummyRow2 ]
                        |> Editor.addRowBeforeAnotherRow dummyRow3 (Editor.rowId dummyRow1)
                        |> Expect.equal { rows = [ dummyRow3, dummyRow1, dummyRow2 ] }
            , test "adds a row after another row as the last" <|
                \_ ->
                    Editor.initWithRows [ dummyRow1, dummyRow2 ]
                        |> Editor.addRowAfterAnotherRow dummyRow3 (Editor.rowId dummyRow2)
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


emailWithContents : List Editor.Content -> Editor.Email
emailWithContents contents =
    Editor.initWithRows
        [ Editor.Row100 (RowId.fromInt 1)
            { id = BlockId.fromInt 1
            , contents = contents
            }
        ]


dummyContent : Int -> Editor.Content
dummyContent id =
    Editor.Button (ContentId.fromInt id)


dummyRow1 : Editor.Row
dummyRow1 =
    Editor.Row100 (RowId.fromInt 1) { id = BlockId.fromInt 1, contents = [] }


dummyRow2 : Editor.Row
dummyRow2 =
    Editor.Row100 (RowId.fromInt 2) { id = BlockId.fromInt 2, contents = [] }


dummyRow3 : Editor.Row
dummyRow3 =
    Editor.Row100 (RowId.fromInt 3) { id = BlockId.fromInt 3, contents = [] }
