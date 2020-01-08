module BlockId exposing (BlockId, decoder, domId, encode, fromInt)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type BlockId
    = BlockId Int


fromInt : Int -> BlockId
fromInt =
    BlockId


domId : BlockId -> String
domId (BlockId id) =
    "block-" ++ String.fromInt id


encode : BlockId -> Value
encode (BlockId id) =
    Encode.int id


decoder : Decoder BlockId
decoder =
    Decode.map BlockId Decode.int
