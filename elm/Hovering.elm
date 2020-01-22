module Hovering exposing (HoverState, HoveringItem(..), clear, init, isHoveringContent, latest, pop, pushContent, pushRow)

import Browser.Dom
import ContentId exposing (ContentId)
import RowId exposing (RowId)


type alias HoverState =
    List HoveringItem


type HoveringItem
    = HoveringRow RowId Browser.Dom.Element
    | HoveringContent ContentId


init : HoverState
init =
    []


clear : HoverState
clear =
    init


pushRow : RowId -> Browser.Dom.Element -> HoverState -> HoverState
pushRow rowId element state =
    if List.member (HoveringRow rowId element) state then
        state

    else
        HoveringRow rowId element :: state


pushContent : ContentId -> HoverState -> HoverState
pushContent contentId state =
    if List.member (HoveringContent contentId) state then
        state

    else
        HoveringContent contentId :: state


pop : HoverState -> HoverState
pop state =
    case state of
        _ :: tail ->
            tail

        [] ->
            []


latest : HoverState -> Maybe HoveringItem
latest =
    List.head


isHoveringContent : ContentId -> HoverState -> Bool
isHoveringContent contentId state =
    state
        |> latest
        |> Maybe.map
            (\item ->
                case item of
                    HoveringContent hoveredContentId ->
                        hoveredContentId == contentId

                    HoveringRow _ _ ->
                        False
            )
        |> Maybe.withDefault False
