port module Interop exposing
    ( UploadDone
    , UploadProgress
    , getContents
    , illustrations
    , openPreview
    , sendContents
    , uploadDone
    , uploadProgress
    )

import Json.Encode exposing (Value)


type alias UploadProgress =
    { percentage : Int
    , contentId : Int
    }


port uploadProgress : (UploadProgress -> msg) -> Sub msg


type alias UploadDone =
    { url : String
    , contentId : Int
    }


port uploadDone : (UploadDone -> msg) -> Sub msg


port illustrations : (Value -> msg) -> Sub msg


port getContents : (() -> msg) -> Sub msg


port sendContents : Value -> Cmd msg


port openPreview : String -> Cmd msg
