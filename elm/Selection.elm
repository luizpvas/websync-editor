module Selection exposing (SelectionState(..), isContentSelected, isRowSelected, selectContent, selectRow)

import ContentId exposing (ContentId)
import RowId exposing (RowId)


type SelectionState
    = Nothing
    | RowSelected RowId
    | ContentSelected ContentId


isContentSelected : ContentId -> SelectionState -> Bool
isContentSelected contentId selection =
    case selection of
        RowSelected _ ->
            False

        ContentSelected selectedId ->
            contentId == selectedId

        Nothing ->
            False


isRowSelected : RowId -> SelectionState -> Bool
isRowSelected rowId selectionState =
    case selectionState of
        RowSelected selectedId ->
            rowId == selectedId

        ContentSelected _ ->
            False

        Nothing ->
            False


selectRow : RowId -> SelectionState
selectRow rowId =
    RowSelected rowId


selectContent : ContentId -> SelectionState
selectContent contentId =
    ContentSelected contentId
