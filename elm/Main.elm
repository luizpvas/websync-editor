module Main exposing (main)

import BlockId exposing (BlockId)
import Browser
import Browser.Dom
import Browser.Events
import Content.Divider as Divider
import Content.Image as Image
import Content.Text as Text
import ContentId exposing (ContentId)
import Dnd
import DomElement
import Editor exposing (Email)
import Error
import File exposing (File)
import Hovering
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Icon
import Interop
import Json.Decode as Decode
import Lang
import RowId exposing (RowId)
import Selection
import Task
import UI



-- Model


type alias Flags =
    { latestId : Int
    }


type alias Model =
    { latestId : Int
    , email : Email
    , drag : Dnd.DragState
    , hover : Hovering.HoverState
    , selection : Selection.SelectionState
    , error : Error.Error
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { latestId = flags.latestId
      , email =
            Editor.emptyEmail
                |> Editor.addFirstContentToBlock (Editor.Image (ContentId.fromInt 2) Image.default) (BlockId.fromInt 1)
      , drag = Dnd.NotDragging
      , hover = Hovering.init
      , selection = Selection.selectContent (ContentId.fromInt 2)
      , error = Error.AllGood
      }
    , Cmd.none
    )



-- Update


type Msg
    = Ignored
      -- Drag and drop & Hovering
    | DragContentStarted String Int Int
    | DragExistingContentStarted ContentId Int Int
    | DragRowStarted String Int Int
    | DragExistingRowStarted RowId Int Int
    | DragMoved Int Int
    | MouseEnteredRow RowId
    | GotElementMouseEnteredRow RowId (Result Browser.Dom.Error Browser.Dom.Element)
    | MouseLeftRow
    | MouseEnteredEmptyBlock BlockId
    | MouseLeftEmptyBlock
    | MouseEnteredContent ContentId
    | GotElementMouseEnteredContent ContentId (Result Browser.Dom.Error Browser.Dom.Element)
    | MouseLeftContent
    | DragStopped
      -- Selection
    | SelectRow RowId
    | SelectContent ContentId
    | ClearSelection
      -- Content: all
    | ContentRemoved ContentId
      -- Content: Image
    | SetImageUploadProgress Interop.UploadProgress
    | SetImageUploadDone Interop.UploadDone
    | SetImageUrl ContentId String
    | SetImageWidth ContentId Image.Width
    | SetImageAlignment ContentId Image.Alignment
    | SetImageAltDescription ContentId String
    | SetImageActionUrl ContentId String
    | SetImagePaddingAll ContentId Int
    | SetImagePaddingTop ContentId Int
    | SetImagePaddingRight ContentId Int
    | SetImagePaddingBottom ContentId Int
    | SetImagePaddingLeft ContentId Int
      -- Content: Divider
    | SetDividerWidthPercentage ContentId Int
    | SetDividerStroke ContentId Divider.Stroke
    | SetDividerThickness ContentId Int
    | SetDividerColor ContentId String
      -- Content: Text
    | SetTextQuill ContentId Text.QuillData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Ignored ->
            ( model, Cmd.none )

        -- ========================
        -- Drag and drop & Hovering
        -- ========================
        DragContentStarted name x y ->
            case Editor.contentFromString name model.latestId of
                Just ( nextId, content ) ->
                    ( { model | drag = Dnd.startDraggingContent content x y, latestId = nextId, hover = Hovering.clear }, Cmd.none )

                Nothing ->
                    flashErrorString "Could not parse content string" model

        DragExistingContentStarted contentId x y ->
            case Editor.findContent contentId model.email of
                Just content ->
                    ( { model | drag = Dnd.startDraggingContent content x y, hover = Hovering.clear }, Cmd.none )

                Nothing ->
                    flashErrorString "Could not find content by ID." model

        DragRowStarted name x y ->
            case Editor.rowFromString name model.latestId of
                Just ( newId, row ) ->
                    ( { model | drag = Dnd.startDraggingRow row x y, latestId = newId, hover = Hovering.clear }, Cmd.none )

                Nothing ->
                    flashErrorString "Could not parse row string" model

        DragExistingRowStarted rowId x y ->
            case Editor.findRow rowId model.email of
                Just row ->
                    ( { model | drag = Dnd.startDraggingRow row x y, hover = Hovering.clear }, Cmd.none )

                Nothing ->
                    flashErrorString "Could not find row by ID." model

        MouseEnteredRow rowId ->
            let
                model1 =
                    { model | hover = Hovering.pushRow rowId model.hover }
            in
            case model.drag of
                Dnd.DraggingRow _ _ _ ->
                    ( model1, Browser.Dom.getElement (RowId.domId rowId) |> Task.attempt (GotElementMouseEnteredRow rowId) )

                Dnd.DraggingContent _ _ _ ->
                    ( model1, Cmd.none )

                Dnd.NotDragging ->
                    ( model1, Cmd.none )

        GotElementMouseEnteredRow rowId result ->
            case result of
                Ok element ->
                    ( { model | drag = Dnd.mapRowTarget rowId (DomElement.fromBrowser element) model.drag }, Cmd.none )

                Err err ->
                    flashErrorDom "Could not find on hover row" err model

        MouseLeftRow ->
            ( { model | drag = Dnd.clearTargetRow model.drag, hover = Hovering.clear }, Cmd.none )

        MouseEnteredEmptyBlock blockId ->
            case model.drag of
                Dnd.DraggingContent content xy _ ->
                    ( { model | drag = Dnd.DraggingContent content xy (Dnd.FirstOfBlock blockId) }, Cmd.none )

                Dnd.DraggingRow _ _ _ ->
                    ( model, Cmd.none )

                Dnd.NotDragging ->
                    ( model, Cmd.none )

        MouseLeftEmptyBlock ->
            ( { model | drag = Dnd.clearTargetContent model.drag }, Cmd.none )

        MouseEnteredContent contentId ->
            let
                model1 =
                    { model | hover = Hovering.pushContent contentId model.hover }
            in
            case model.drag of
                Dnd.DraggingRow _ _ _ ->
                    ( model1, Cmd.none )

                Dnd.DraggingContent _ _ _ ->
                    ( model1
                    , Browser.Dom.getElement (ContentId.domId contentId) |> Task.attempt (GotElementMouseEnteredContent contentId)
                    )

                Dnd.NotDragging ->
                    ( model1, Cmd.none )

        GotElementMouseEnteredContent contentId result ->
            case result of
                Ok element ->
                    ( { model | drag = Dnd.mapContentTarget contentId (DomElement.fromBrowser element) model.drag }, Cmd.none )

                Err err ->
                    flashErrorDom "Could not find DOM element" err model

        MouseLeftContent ->
            ( { model | drag = Dnd.clearTargetContent model.drag, hover = Hovering.pop model.hover }, Cmd.none )

        DragMoved x y ->
            ( { model | drag = Dnd.mapCoordinate { x = x, y = y } model.drag }, Cmd.none )

        DragStopped ->
            let
                newSelection =
                    case model.drag of
                        Dnd.DraggingRow row _ _ ->
                            Selection.selectRow (Editor.rowId row)

                        Dnd.DraggingContent content _ _ ->
                            Selection.selectContent (Editor.contentId content)

                        Dnd.NotDragging ->
                            Selection.Nothing
            in
            ( { model
                | drag = Dnd.NotDragging
                , email = Dnd.drop model.email model.drag
                , selection = newSelection
              }
            , Cmd.none
            )

        -- ========================
        -- Selection
        -- ========================
        SelectRow rowId ->
            ( { model | selection = Selection.selectRow rowId }, Cmd.none )

        SelectContent contentId ->
            ( { model | selection = Selection.selectContent contentId }, Cmd.none )

        ClearSelection ->
            ( { model | selection = Selection.Nothing }, Cmd.none )

        -- ========================
        -- Content: All
        -- ========================
        ContentRemoved contentId ->
            ( { model | email = Editor.removeContent contentId model.email, selection = Selection.Nothing }, Cmd.none )

        -- ========================
        -- Content: Image
        -- ========================
        SetImageUploadProgress progress ->
            mapImage (ContentId.fromInt progress.contentId) (Image.mapUploadProgress progress.percentage) model

        SetImageUploadDone done ->
            mapImage (ContentId.fromInt done.contentId) (Image.mapUrl done.url) model

        SetImageUrl id url ->
            mapImage id (Image.mapUrl url) model

        SetImageWidth id width ->
            mapImage id (Image.mapWidth width) model

        SetImageAlignment id alignment ->
            mapImage id (Image.mapAlignment alignment) model

        SetImageAltDescription id altDescription ->
            mapImage id (Image.mapAltDescription altDescription) model

        SetImageActionUrl id actionUrl ->
            mapImage id (Image.mapActionUrl actionUrl) model

        SetImagePaddingAll id padding ->
            mapImage id (Image.mapPaddingAll padding) model

        SetImagePaddingTop id top ->
            mapImage id (Image.mapPaddingTop top) model

        SetImagePaddingRight id right ->
            mapImage id (Image.mapPaddingRight right) model

        SetImagePaddingBottom id bottom ->
            mapImage id (Image.mapPaddingBottom bottom) model

        SetImagePaddingLeft id left ->
            mapImage id (Image.mapPaddingLeft left) model

        -- ========================
        -- Content: Divider
        -- ========================
        SetDividerWidthPercentage id width ->
            mapDivider id (Divider.mapWidthPercentage width) model

        SetDividerStroke id stroke ->
            mapDivider id (Divider.mapStroke stroke) model

        SetDividerThickness id thickness ->
            mapDivider id (Divider.mapThickness thickness) model

        SetDividerColor id color ->
            mapDivider id (Divider.mapColor color) model

        -- ========================
        -- Content: Text
        -- ========================
        SetTextQuill id data ->
            mapText id (Text.mapQuillEvent data) model


flashErrorDom : String -> Browser.Dom.Error -> Model -> ( Model, Cmd Msg )
flashErrorDom description error model =
    ( { model | error = Error.DomError description error }, Cmd.none )


flashErrorString : String -> Model -> ( Model, Cmd Msg )
flashErrorString description model =
    ( { model | error = Error.StringError description }, Cmd.none )


mapImage : ContentId -> (Image.Image -> Image.Image) -> Model -> ( Model, Cmd Msg )
mapImage id fn model =
    ( { model | email = Editor.mapImage id fn model.email }, Cmd.none )


mapDivider : ContentId -> (Divider.Divider -> Divider.Divider) -> Model -> ( Model, Cmd Msg )
mapDivider id fn model =
    ( { model | email = Editor.mapDivider id fn model.email }, Cmd.none )


mapText : ContentId -> (Text.Text -> Text.Text) -> Model -> ( Model, Cmd Msg )
mapText id fn model =
    ( { model | email = Editor.mapText id fn model.email }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    let
        appClass =
            if Dnd.isDragging model.drag then
                "ws-app is-dragging"

            else
                "ws-app"
    in
    div [ class appClass ]
        [ viewEditor model
        , div [ class "ws-right-panel" ] [ viewRightPanel model ]
        , viewDraggingItem model.drag
        , viewDraggingTarget model.drag
        ]


viewEditor : Model -> Html Msg
viewEditor model =
    div [ class "ws-editor ws-rt-email", onClick ClearSelection ]
        (List.map (viewEditorRow model.drag model.hover model.selection) model.email.rows)


viewEditorRow : Dnd.DragState -> Hovering.HoverState -> Selection.SelectionState -> Editor.Row -> Html Msg
viewEditorRow dragState hoverState selection row =
    case row of
        Editor.Row100 rowId block ->
            div
                [ class "ws-row"
                , id (RowId.domId rowId)
                , onMouseEnter (MouseEnteredRow rowId)
                , onMouseLeave MouseLeftRow
                , stopPropagationOn "click" (Decode.succeed ( SelectRow rowId, True ))
                ]
                [ viewEditorRowControls dragState hoverState selection row
                , viewEditorBlock dragState hoverState selection block
                ]

        _ ->
            text ""


viewEditorRowControls : Dnd.DragState -> Hovering.HoverState -> Selection.SelectionState -> Editor.Row -> Html Msg
viewEditorRowControls drag hoverState selection row =
    if Dnd.isDragging drag then
        text ""

    else if Selection.isRowSelected (Editor.rowId row) selection then
        div [ class "ws-row-hover ws-selected" ]
            [ div
                [ class "ws-move-icon"
                , Dnd.onMouseDown (DragExistingRowStarted (Editor.rowId row))
                ]
                [ Icon.move ]
            ]

    else if Hovering.isHoveringRow (Editor.rowId row) hoverState then
        div [ class "ws-row-hover" ]
            [ div
                [ class "ws-move-icon"
                , Dnd.onMouseDown (DragExistingRowStarted (Editor.rowId row))
                ]
                [ Icon.move ]
            ]

    else
        text ""


viewEditorBlock : Dnd.DragState -> Hovering.HoverState -> Selection.SelectionState -> Editor.Block -> Html Msg
viewEditorBlock dragState hoverState selection block =
    if List.isEmpty block.contents then
        viewEmptyBlock dragState block.id

    else
        Html.Keyed.node "div" [ id (BlockId.domId block.id) ] (List.map (viewEditorBlockContent dragState hoverState selection) block.contents)


viewEmptyBlock : Dnd.DragState -> BlockId -> Html Msg
viewEmptyBlock dragState blockId =
    let
        className =
            case dragState of
                Dnd.DraggingContent _ _ (Dnd.FirstOfBlock targetBlockId) ->
                    if targetBlockId == blockId then
                        "ws-empty-block drop-target"

                    else
                        "ws-empty-block"

                _ ->
                    "ws-empty-block"
    in
    div
        [ class className
        , id (BlockId.domId blockId)
        , onMouseEnter (MouseEnteredEmptyBlock blockId)
        , onMouseLeave MouseLeftEmptyBlock
        ]
        [ text Lang.addFirstComponent ]


viewEditorBlockContent : Dnd.DragState -> Hovering.HoverState -> Selection.SelectionState -> Editor.Content -> ( String, Html Msg )
viewEditorBlockContent drag hoverState selection content =
    let
        contentView =
            if Selection.isContentSelected (Editor.contentId content) selection then
                ( "selected-" ++ ContentId.domId (Editor.contentId content), renderSelectedContent content )

            else
                ( "rendered-" ++ ContentId.domId (Editor.contentId content), renderContent content )
    in
    ( ContentId.domId (Editor.contentId content)
    , Html.Keyed.node "div"
        [ class "ws-content"
        , id (ContentId.domId (Editor.contentId content))
        , onMouseEnter (MouseEnteredContent (Editor.contentId content))
        , onMouseLeave MouseLeftContent
        , stopPropagationOn "click" (Decode.succeed ( SelectContent (Editor.contentId content), True ))
        ]
        [ ( "controls-" ++ ContentId.domId (Editor.contentId content)
          , viewEditorBlockContentControls drag hoverState selection content
          )
        , contentView
        ]
    )


renderContent : Editor.Content -> Html msg
renderContent content =
    case content of
        Editor.Button id ->
            text "Render Button..."

        Editor.Divider id divider ->
            text "Render Divider..."

        Editor.Image id image ->
            text "Render Image..."

        Editor.Text id editingText ->
            node "websync-html" [ attribute "data-html" editingText.html ] []

        Editor.YoutubeVideo id ->
            text "Render youtube video"


renderSelectedContent : Editor.Content -> Html Msg
renderSelectedContent content =
    case content of
        Editor.Button id ->
            text "Render selected Button..."

        Editor.Divider id divider ->
            text "Render selected Divider..."

        Editor.Image id image ->
            text "Render selected Image..."

        Editor.Text id text ->
            Text.quill (SetTextQuill id) text

        Editor.YoutubeVideo id ->
            text "Render selected youtube video"


viewEditorBlockContentControls : Dnd.DragState -> Hovering.HoverState -> Selection.SelectionState -> Editor.Content -> Html Msg
viewEditorBlockContentControls drag hoverState selection content =
    if Dnd.isDragging drag then
        text ""

    else if Selection.isContentSelected (Editor.contentId content) selection then
        div [ class "ws-content-hover ws-selected" ]
            [ div
                [ class "ws-move-icon"
                , Dnd.onMouseDown (DragExistingContentStarted (Editor.contentId content))
                ]
                [ Icon.move ]
            ]

    else if Hovering.isHoveringContent (Editor.contentId content) hoverState then
        div [ class "ws-content-hover" ]
            [ div
                [ class "ws-move-icon"
                , Dnd.onMouseDown (DragExistingContentStarted (Editor.contentId content))
                ]
                [ Icon.move ]
            ]

    else
        text ""


viewRightPanel : Model -> Html Msg
viewRightPanel model =
    case model.selection of
        Selection.Nothing ->
            viewDraggableItems model

        Selection.RowSelected rowId ->
            text "Row selected..."

        Selection.ContentSelected contentId ->
            case Editor.findContent contentId model.email of
                Just content ->
                    case content of
                        Editor.Button id ->
                            text "Button..."

                        Editor.Divider id divider ->
                            Divider.editor
                                { divider = divider
                                , remove = ContentRemoved id
                                , close = ClearSelection
                                , setWidthPercentage = SetDividerWidthPercentage id
                                , setStroke = SetDividerStroke id
                                , setThickness = SetDividerThickness id
                                , setColor = SetDividerColor id
                                }

                        Editor.Image id image ->
                            Image.editor
                                { image = image
                                , contentId = id
                                , remove = ContentRemoved id
                                , close = ClearSelection
                                , setUrl = SetImageUrl id
                                , setWidth = SetImageWidth id
                                , setAlignment = SetImageAlignment id
                                , setAltDescription = SetImageAltDescription id
                                , setActionUrl = SetImageActionUrl id
                                , setPaddingTop = SetImagePaddingTop id
                                , setPaddingRight = SetImagePaddingRight id
                                , setPaddingBottom = SetImagePaddingBottom id
                                , setPaddingLeft = SetImagePaddingLeft id
                                , setPaddingAll = SetImagePaddingAll id
                                }

                        Editor.Text id editingText ->
                            text "Text content"

                        Editor.YoutubeVideo id ->
                            text "Youtube video"

                Nothing ->
                    text "??"


viewDraggableItems : Model -> Html Msg
viewDraggableItems model =
    div [ class "ws-draggable-items" ]
        [ viewDraggableContent "Text" [ Icon.text, text Lang.text ]
        , viewDraggableContent "Image" [ Icon.image, text Lang.image ]
        , viewDraggableContent "Button" [ Icon.button, text Lang.button ]
        , viewDraggableContent "Divider" [ Icon.divider, text Lang.divider ]
        , viewDraggableContent "YoutubeVideo" [ Icon.youtube, text Lang.youtubeVideo ]
        , UI.divider
        , viewDraggableRow "Row100" [ text "Row100" ]
        , viewDraggableRow "Row50x50" [ text "Row50x50" ]
        ]


viewDraggableContent : String -> List (Html Msg) -> Html Msg
viewDraggableContent contentName children =
    div
        [ class "ws-draggable-item"
        , Dnd.onMouseDown (DragContentStarted contentName)
        ]
        children


viewDraggableRow : String -> List (Html Msg) -> Html Msg
viewDraggableRow rowName children =
    div
        [ class "ws-draggable-item"
        , Dnd.onMouseDown (DragRowStarted rowName)
        ]
        children


{-| Renders the item being dragged, if there is one. The item os absolute
positioned based on the dragging coordinate, so it doesn't matter where
in the DOM tree this function is called.
-}
viewDraggingItem : Dnd.DragState -> Html Msg
viewDraggingItem drag =
    case drag of
        Dnd.DraggingRow row coordinate target ->
            div
                [ class "ws-item-being-dragged"
                , style "top" (String.fromInt coordinate.y ++ "px")
                , style "left" (String.fromInt coordinate.x ++ "px")
                ]
                [ text "Block" ]

        Dnd.DraggingContent item coordinate target ->
            div
                [ class "ws-item-being-dragged"
                , style "top" (String.fromInt coordinate.y ++ "px")
                , style "left" (String.fromInt coordinate.x ++ "px")
                ]
                [ text "Content" ]

        Dnd.NotDragging ->
            text ""


viewDraggingTarget : Dnd.DragState -> Html Msg
viewDraggingTarget dragState =
    case dragState of
        Dnd.DraggingRow item xy target ->
            case target of
                Dnd.AfterRow _ element ->
                    div
                        [ class "ws-drop-target"
                        , style "top" (String.fromInt (element.y + element.height) ++ "px")
                        , style "left" (String.fromInt element.x ++ "px")
                        , style "width" (String.fromInt element.width ++ "px")
                        ]
                        [ div [ class "ws-drop-target-text" ] [ text "Drop here" ] ]

                Dnd.BeforeRow row element ->
                    div
                        [ class "ws-drop-target"
                        , style "top" (String.fromInt element.y ++ "px")
                        , style "left" (String.fromInt element.x ++ "px")
                        , style "width" (String.fromInt element.width ++ "px")
                        ]
                        [ div [ class "ws-drop-target-text" ] [ text "Drop here" ] ]

                Dnd.NoTargetRow ->
                    text ""

        Dnd.DraggingContent item xy target ->
            case target of
                Dnd.AfterContent _ element ->
                    div
                        [ class "ws-drop-target"
                        , style "top" (String.fromInt (element.y + element.height) ++ "px")
                        , style "left" (String.fromInt element.x ++ "px")
                        , style "width" (String.fromInt element.width ++ "px")
                        ]
                        [ div [ class "ws-drop-target-text" ] [ text "Drop here" ] ]

                Dnd.BeforeContent row element ->
                    div
                        [ class "ws-drop-target"
                        , style "top" (String.fromInt element.y ++ "px")
                        , style "left" (String.fromInt element.x ++ "px")
                        , style "width" (String.fromInt element.width ++ "px")
                        ]
                        [ div [ class "ws-drop-target-text" ] [ text "Drop here" ] ]

                Dnd.FirstOfBlock _ ->
                    text ""

                Dnd.NoTargetContent ->
                    text ""

        Dnd.NotDragging ->
            text ""



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        mouseMoveForDragging =
            if Dnd.isDragging model.drag then
                Browser.Events.onMouseMove <|
                    Decode.map2 DragMoved
                        (Decode.field "pageX" Decode.int)
                        (Decode.field "pageY" Decode.int)

            else
                Sub.none

        mouseUpForDragging =
            if Dnd.isDragging model.drag then
                Browser.Events.onMouseUp (Decode.succeed DragStopped)

            else
                Sub.none
    in
    Sub.batch
        [ mouseMoveForDragging
        , mouseUpForDragging
        , Interop.uploadProgress SetImageUploadProgress
        , Interop.uploadDone SetImageUploadDone
        ]



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
