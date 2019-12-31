module Hovering exposing (HoverState, HoveringItem(..), clear, init, isHoveringContent, isHoveringRow, pop, pushContent, pushRow)

import ContentId exposing (ContentId)
import RowId exposing (RowId)


type alias HoverState =
    List HoveringItem


type HoveringItem
    = HoveringRow RowId
    | HoveringContent ContentId


init : HoverState
init =
    []


clear : HoverState
clear =
    init


pushRow : RowId -> HoverState -> HoverState
pushRow rowId state =
    if List.member (HoveringRow rowId) state then
        state

    else
        HoveringRow rowId :: state


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


isHoveringContent : ContentId -> HoverState -> Bool
isHoveringContent contentId state =
    state
        |> List.head
        |> Maybe.map
            (\item ->
                case item of
                    HoveringContent hoveredContentId ->
                        hoveredContentId == contentId

                    HoveringRow _ ->
                        False
            )
        |> Maybe.withDefault False


isHoveringRow : RowId -> HoverState -> Bool
isHoveringRow rowId state =
    state
        |> List.head
        |> Maybe.map
            (\item ->
                case item of
                    HoveringRow hoveredRowId ->
                        hoveredRowId == rowId

                    HoveringContent _ ->
                        False
            )
        |> Maybe.withDefault False
