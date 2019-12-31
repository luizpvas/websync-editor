module ContentId exposing (ContentId(..), domId, fromInt, toString)


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
    "row-" ++ String.fromInt id
