module DndTest exposing (suite)

import Dnd
import DomElement exposing (DomElement)
import Editor
import Expect
import RowId exposing (RowId)
import Test exposing (..)



-- 0 1 2 3 4 5
-- 1
-- 2   * * *
-- 3   * * *
-- 4   * * *
-- 5


suite : Test
suite =
    describe "calculating the target position"
        [ test "detects out of rect" <|
            \_ ->
                Expect.equal
                    (Dnd.calculateRowTarget { x = 1, y = 1 } dummyRowId dummyElement)
                    Dnd.NoTargetRow
        , test "detects before rect if closer to top" <|
            \_ ->
                Expect.equal
                    (Dnd.calculateRowTarget { x = 2, y = 2 } dummyRowId dummyElement)
                    (Dnd.BeforeRow dummyRowId dummyElement)
        , test "detects after rect if closer to bottom" <|
            \_ ->
                Expect.equal
                    (Dnd.calculateRowTarget { x = 2, y = 4 } dummyRowId dummyElement)
                    (Dnd.AfterRow dummyRowId dummyElement)
        ]


dummyElement : DomElement
dummyElement =
    { x = 2, y = 2, width = 2, height = 2 }


dummyRowId : RowId
dummyRowId =
    RowId.fromInt 1
