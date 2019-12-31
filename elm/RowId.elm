module RowId exposing (RowId(..), domId, fromInt)


type RowId
    = RowId Int


fromInt : Int -> RowId
fromInt =
    RowId


domId : RowId -> String
domId (RowId id) =
    "row-" ++ String.fromInt id
