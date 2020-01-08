module Block exposing (Block, decoder, encode)

import BlockId exposing (BlockId)
import Content exposing (ContentList)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Block =
    { id : BlockId
    , width : Float
    , contents : ContentList
    }


encode : Block -> Value
encode block =
    Encode.object
        [ ( "id", BlockId.encode block.id )
        , ( "width", Encode.float block.width )
        , ( "contents", Encode.list Content.encode block.contents )
        ]


decoder : Decoder Block
decoder =
    Decode.map3 Block
        (Decode.field "id" BlockId.decoder)
        (Decode.field "width" Decode.float)
        (Decode.field "contents" (Decode.list Content.decoder))
