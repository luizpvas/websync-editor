module EditHistory exposing (EditHistory, canRedo, canUndo, hasUnsavedChanges, init, markSavedPoint, push, redo, undo)


type alias EditHistory a =
    { history : List a
    , future : List a
    , latestSave : Maybe a
    }


type Action
    = Once
    | Repeated


init : a -> EditHistory a
init a =
    { history = [ a ]
    , future = []
    , latestSave = Just a
    }


push : a -> EditHistory a -> EditHistory a
push item edit =
    case edit.history of
        latest :: rest ->
            if item == latest then
                edit

            else
                { edit | history = item :: latest :: rest, future = [] }

        [] ->
            { edit | history = [ item ], future = [] }


markSavedPoint : a -> EditHistory a -> EditHistory a
markSavedPoint savePoint edit =
    { edit | latestSave = Just savePoint }


undo : EditHistory a -> ( Maybe a, EditHistory a )
undo edit =
    case edit.history of
        current :: previous :: rest ->
            ( Just previous
            , { edit
                | history = previous :: rest
                , future = current :: edit.future
              }
            )

        _ ->
            ( Nothing, edit )


redo : EditHistory a -> ( Maybe a, EditHistory a )
redo edit =
    case edit.future of
        item :: rest ->
            ( Just item, { edit | history = item :: edit.history, future = rest } )

        [] ->
            ( Nothing, edit )


hasUnsavedChanges : EditHistory a -> Bool
hasUnsavedChanges editHistory =
    case editHistory.latestSave of
        Nothing ->
            List.length editHistory.history > 0

        Just saved ->
            case editHistory.history of
                latest :: _ ->
                    latest /= saved

                [] ->
                    False


canUndo : EditHistory a -> Bool
canUndo edit =
    List.length edit.history > 1


canRedo : EditHistory a -> Bool
canRedo edit =
    List.length edit.future > 0
