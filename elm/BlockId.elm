module BlockId exposing (BlockId, domId, fromInt)


type BlockId
    = BlockId Int


fromInt : Int -> BlockId
fromInt =
    BlockId


domId : BlockId -> String
domId (BlockId id) =
    "block-" ++ String.fromInt id
