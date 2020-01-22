module Selection exposing (SelectionState(..), isContentSelected, selectContent, selectRow)

import Browser.Dom
import ContentId exposing (ContentId)
import RowId exposing (RowId)


type SelectionState
    = Nothing
    | RowSelected RowId Browser.Dom.Element
    | ContentSelected ContentId


isContentSelected : ContentId -> SelectionState -> Bool
isContentSelected contentId selection =
    case selection of
        RowSelected _ _ ->
            False

        ContentSelected selectedId ->
            contentId == selectedId

        Nothing ->
            False


selectRow : RowId -> Browser.Dom.Element -> SelectionState
selectRow rowId element =
    RowSelected rowId element


selectContent : ContentId -> SelectionState
selectContent contentId =
    ContentSelected contentId
