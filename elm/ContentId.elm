module ContentId exposing (ContentId(..), decoder, domId, encode, fromInt, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type ContentId
    = ContentId Int


fromInt : Int -> ContentId
fromInt =
    ContentId


toString : ContentId -> String
toString (ContentId id) =
    String.fromInt id


domId : ContentId -> String
domId (ContentId id) =
    "content-" ++ String.fromInt id


encode : ContentId -> Value
encode (ContentId id) =
    Encode.int id


decoder : Decoder ContentId
decoder =
    Decode.map ContentId Decode.int
