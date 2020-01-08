module RowId exposing (RowId(..), decoder, domId, encode, fromInt)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type RowId
    = RowId Int


fromInt : Int -> RowId
fromInt =
    RowId


domId : RowId -> String
domId (RowId id) =
    "row-" ++ String.fromInt id


encode : RowId -> Value
encode (RowId id) =
    Encode.int id


decoder : Decoder RowId
decoder =
    Decode.map RowId Decode.int
