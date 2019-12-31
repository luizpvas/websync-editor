module Dnd exposing
    ( ContentTarget(..)
    , DragState(..)
    , RowTarget(..)
    , calculateRowTarget
    , clearTargetContent
    , clearTargetRow
    , drop
    , isDragging
    , mapContentTarget
    , mapCoordinate
    , mapRowTarget
    , onMouseDown
    , startDraggingContent
    , startDraggingRow
    )

import BlockId exposing (BlockId)
import ContentId exposing (ContentId)
import DomElement exposing (DomElement)
import Editor
import Html exposing (Attribute)
import Html.Events exposing (preventDefaultOn)
import Json.Decode as Decode
import RowId exposing (RowId)



-- Helpers


onMouseDown : (Int -> Int -> msg) -> Attribute msg
onMouseDown toMsg =
    let
        dragStartDecoder =
            Decode.map2 toMsg
                (Decode.field "pageX" Decode.int)
                (Decode.field "pageY" Decode.int)
                |> Decode.map (\msg -> ( msg, True ))
    in
    preventDefaultOn "mousedown" dragStartDecoder



-- Data


type DragState
    = NotDragging
    | DraggingRow Editor.Row Coordinate RowTarget
    | DraggingContent Editor.Content Coordinate ContentTarget


type alias Coordinate =
    { x : Int
    , y : Int
    }


type RowTarget
    = NoTargetRow
    | BeforeRow RowId DomElement
    | AfterRow RowId DomElement


type ContentTarget
    = NoTargetContent
    | BeforeContent ContentId DomElement
    | AfterContent ContentId DomElement
    | FirstOfBlock BlockId


startDraggingContent : Editor.Content -> Int -> Int -> DragState
startDraggingContent content x y =
    DraggingContent content { x = x, y = y } NoTargetContent


startDraggingRow : Editor.Row -> Int -> Int -> DragState
startDraggingRow row x y =
    DraggingRow row { x = x, y = y } NoTargetRow


drop : Editor.Email -> DragState -> Editor.Email
drop email dragState =
    case dragState of
        DraggingRow row _ target ->
            case target of
                AfterRow afterRow _ ->
                    if Editor.rowId row == afterRow then
                        -- If we're dragging the row before/after itself,
                        -- there is nothing to do.
                        email

                    else
                        email
                            |> Editor.removeRow (Editor.rowId row)
                            |> Editor.addRowAfterAnotherRow row afterRow

                BeforeRow beforeRow _ ->
                    if Editor.rowId row == beforeRow then
                        -- If we're dragging the row before/after itself,
                        -- there is nothing to do.
                        email

                    else
                        email
                            |> Editor.removeRow (Editor.rowId row)
                            |> Editor.addRowBeforeAnotherRow row beforeRow

                NoTargetRow ->
                    email

        DraggingContent item xy target ->
            case target of
                AfterContent afterId _ ->
                    if Editor.contentId item == afterId then
                        -- If we're dragging the content after/before itself,
                        -- there is no work to do.
                        email

                    else
                        email
                            |> Editor.removeContent (Editor.contentId item)
                            |> Editor.addContentAfterAnotherContent item afterId

                BeforeContent beforeId _ ->
                    if Editor.contentId item == beforeId then
                        -- If we're dragging the content after/before itself,
                        -- there is no work to do.
                        email

                    else
                        email
                            |> Editor.removeContent (Editor.contentId item)
                            |> Editor.addContentBeforeAnotherContent item beforeId

                FirstOfBlock blockId ->
                    email
                        |> Editor.removeContent (Editor.contentId item)
                        |> Editor.addFirstContentToBlock item blockId

                NoTargetContent ->
                    email

        NotDragging ->
            email


{-| Clears the target of the item being dragged. This function is called
on mouse leave of possible targets.
-}
clearTargetRow : DragState -> DragState
clearTargetRow dragState =
    case dragState of
        DraggingRow row coordinate _ ->
            DraggingRow row coordinate NoTargetRow

        DraggingContent _ _ _ ->
            dragState

        NotDragging ->
            dragState


clearTargetContent : DragState -> DragState
clearTargetContent dragState =
    case dragState of
        DraggingRow _ _ _ ->
            dragState

        DraggingContent content coordinate _ ->
            DraggingContent content coordinate NoTargetContent

        NotDragging ->
            dragState


{-| Updates the x and y coordinates of the item being dragged. This function is
used with `Browser.Events.onMouseMove` to get the cursor position and update
the coordinate. The item is displayed using absolute positioning.
-}
mapCoordinate : Coordinate -> DragState -> DragState
mapCoordinate xy state =
    case state of
        DraggingRow row _ target ->
            case target of
                BeforeRow beforeRow element ->
                    DraggingRow row xy (calculateRowTarget xy beforeRow element)

                AfterRow afterRow element ->
                    DraggingRow row xy (calculateRowTarget xy afterRow element)

                NoTargetRow ->
                    DraggingRow row xy NoTargetRow

        DraggingContent content _ target ->
            case target of
                BeforeContent beforeRow element ->
                    DraggingContent content xy (calculateContentTarget xy beforeRow element)

                AfterContent afterRow element ->
                    DraggingContent content xy (calculateContentTarget xy afterRow element)

                FirstOfBlock block ->
                    DraggingContent content xy (FirstOfBlock block)

                NoTargetContent ->
                    DraggingContent content xy NoTargetContent

        NotDragging ->
            NotDragging


{-| Updates the dragging row target based on the given hovered row. This function
is called on mouseenter for rows on the screen.
-}
mapRowTarget : RowId -> DomElement -> DragState -> DragState
mapRowTarget rowId element dragState =
    case dragState of
        DraggingRow item xy _ ->
            DraggingRow item xy (calculateRowTarget xy rowId element)

        DraggingContent _ _ _ ->
            dragState

        NotDragging ->
            dragState


mapContentTarget : ContentId -> DomElement -> DragState -> DragState
mapContentTarget contentId element dragState =
    case dragState of
        DraggingRow _ _ _ ->
            dragState

        DraggingContent item xy _ ->
            DraggingContent item xy (calculateContentTarget xy contentId element)

        NotDragging ->
            dragState


{-| Checks if the we're dragging something at the moment, whether it's
row or content.
-}
isDragging : DragState -> Bool
isDragging state =
    case state of
        NotDragging ->
            False

        DraggingRow _ _ _ ->
            True

        DraggingContent _ _ _ ->
            True


{-| Calculates the target based on the dragging position and the targed being
hovered.
-}
calculateRowTarget : Coordinate -> RowId -> DomElement -> RowTarget
calculateRowTarget dragXY rowId element =
    let
        isInside =
            (dragXY.x >= element.x)
                && (dragXY.y >= element.y)
                && (dragXY.x <= (element.x + element.width))
                && (dragXY.y <= (element.y + element.height))

        middleLine =
            element.y + (element.height // 2)

        isCloserToTop =
            dragXY.y < middleLine
    in
    if isInside then
        if isCloserToTop then
            BeforeRow rowId element

        else
            AfterRow rowId element

    else
        NoTargetRow


calculateContentTarget : Coordinate -> ContentId -> DomElement -> ContentTarget
calculateContentTarget dragXY contentId element =
    let
        isInside =
            (dragXY.x >= element.x)
                && (dragXY.y >= element.y)
                && (dragXY.x <= (element.x + element.width))
                && (dragXY.y <= (element.y + element.height))

        middleLine =
            element.y + (element.height // 2)

        isCloserToTop =
            dragXY.y < middleLine
    in
    if isInside then
        if isCloserToTop then
            BeforeContent contentId element

        else
            AfterContent contentId element

    else
        NoTargetContent
