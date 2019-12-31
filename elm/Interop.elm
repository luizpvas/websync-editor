port module Interop exposing (UploadDone, UploadProgress, uploadDone, uploadProgress)


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
