module Hovering exposing (HoverState(..), clear, init, isHoveringContent, pop, pushContent, pushRow)

import Browser.Dom
import ContentId exposing (ContentId)
import RowId exposing (RowId)


type HoverState
    = NotHovering
    | HoveringRow RowId Browser.Dom.Element
    | HoveringContentWaitingForRowElement ContentId
    | HoveringContent ContentId RowId Browser.Dom.Element


init : HoverState
init =
    NotHovering


clear : HoverState
clear =
    init


pushRow : RowId -> Browser.Dom.Element -> HoverState -> HoverState
pushRow rowId element state =
    case state of
        NotHovering ->
            HoveringRow rowId element

        HoveringRow _ _ ->
            HoveringRow rowId element

        HoveringContentWaitingForRowElement contentId ->
            HoveringContent contentId rowId element

        HoveringContent _ _ _ ->
            HoveringRow rowId element


pushContent : ContentId -> HoverState -> HoverState
pushContent contentId state =
    case state of
        NotHovering ->
            HoveringContentWaitingForRowElement contentId

        HoveringRow rowId element ->
            HoveringContent contentId rowId element

        HoveringContentWaitingForRowElement _ ->
            HoveringContentWaitingForRowElement contentId

        HoveringContent _ rowId element ->
            HoveringContent contentId rowId element


pop : HoverState -> HoverState
pop state =
    case state of
        NotHovering ->
            NotHovering

        HoveringRow _ _ ->
            NotHovering

        HoveringContentWaitingForRowElement _ ->
            NotHovering

        HoveringContent _ rowId element ->
            HoveringRow rowId element


isHoveringContent : ContentId -> HoverState -> Bool
isHoveringContent contentId state =
    case state of
        HoveringContent hoveringContentId _ _ ->
            contentId == hoveringContentId

        _ ->
            False
