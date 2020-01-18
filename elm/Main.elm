module Main exposing (main)

import Block exposing (Block)
import BlockId exposing (BlockId)
import Browser
import Browser.Dom
import Browser.Events
import Content exposing (Content)
import Content.Button as Button
import Content.Divider as Divider
import Content.Image as Image
import Content.Text as Text
import ContentId exposing (ContentId)
import Dnd
import DomElement
import Dropdown
import EditHistory exposing (EditHistory)
import Editor exposing (Email)
import Error
import Hovering
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Icon
import Illustration exposing (Illustration)
import Interop
import Json.Decode as Decode
import Json.Encode exposing (Value)
import Lang
import Padding
import RawHtml
import Row exposing (Row)
import RowId exposing (RowId)
import Selection
import Task
import Trix
import UI



-- Model


type alias Flags =
    { latestId : Int
    , mouseOffsetX : Int
    , mouseOffsetY : Int
    , serialized : Value
    }


type alias Editor =
    { email : Email
    , selection : Selection.SelectionState
    }


type alias Model =
    { latestId : Int
    , editor : Editor
    , editHistory : EditHistory Editor
    , illustrations : List Illustration
    , mouseOffsetX : Int
    , mouseOffsetY : Int
    , drag : Dnd.DragState
    , hover : Hovering.HoverState
    , error : Error.Error
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        email =
            Decode.decodeValue Editor.decoder flags.serialized
                |> Result.toMaybe
                |> Maybe.withDefault Editor.emptyEmail

        editor =
            { email = email
            , selection = Selection.Nothing
            }
    in
    ( { latestId = flags.latestId
      , editor = editor
      , editHistory = EditHistory.init editor
      , illustrations = []
      , mouseOffsetX = flags.mouseOffsetX
      , mouseOffsetY = flags.mouseOffsetY
      , drag = Dnd.NotDragging
      , hover = Hovering.init
      , error = Error.AllGood
      }
    , Cmd.none
    )



-- Update


type Msg
    = Ignored
      -- Model
    | Load Value
    | GotIllustrations Value
    | EncodeAndSendContents
    | Save
    | OpenPreview String
    | Undo
    | Redo
      -- Drag and drop & Hovering
    | DragContentStarted String Int Int
    | DragExistingContentStarted ContentId Int Int
    | DragRowStarted String Int Int
    | DragExistingRowStarted RowId Int Int
    | DragBlockResizerStarted RowId BlockId BlockId
    | GotElementBlockResizerRow RowId BlockId BlockId (Result Browser.Dom.Error Browser.Dom.Element)
    | DragMoved Int Int Int Int
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
      -- Editing row
    | RemoveRow RowId
    | SetRowBackground RowId String
    | SetRowPadding RowId Padding.Msg
    | SetRowAlignment RowId Row.Alignment
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
    | SetImagePadding ContentId Padding.Msg
    | TransitionImageToIllustrationPicker ContentId
    | CloseImageIllustrationsPicker ContentId
    | SetImageIllustrationsSearch ContentId String
    | SetImageIllustrationsColor ContentId String
    | PickIllustrationForImage ContentId String
      -- Content: Divider
    | SetDividerWidthPercentage ContentId Int
    | SetDividerStroke ContentId Divider.Stroke
    | SetDividerThickness ContentId Int
    | SetDividerColor ContentId String
    | SetDividerPadding ContentId Padding.Msg
      -- Content: Text
    | SetTextTrix ContentId Trix.Model
    | SetTextPadding ContentId Padding.Msg
      -- Content: Button
    | SetButtonTrix ContentId Trix.Model
    | SetButtonUrl ContentId String
    | SetButtonPadding ContentId Padding.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Ignored ->
            ( model, Cmd.none )

        Load value ->
            case Decode.decodeValue Editor.decoder value of
                Ok email ->
                    ( { model | editHistory = EditHistory.init { email = email, selection = Selection.Nothing } }
                        |> mapEditor (\_ -> { email = email, selection = Selection.Nothing })
                    , Cmd.none
                    )

                Err err ->
                    flashErrorString "Failed to decode editor on load." model

        GotIllustrations value ->
            case Decode.decodeValue (Decode.list Illustration.decoder) value of
                Ok illustrations ->
                    ( { model | illustrations = illustrations }, Cmd.none )

                Err err ->
                    flashErrorString "Failed to decode illustrations list." model

        EncodeAndSendContents ->
            ( mapEditor (\editor -> { editor | selection = Selection.Nothing }) model
            , Interop.sendContents (Editor.encode model.editor.email)
            )

        Save ->
            ( mapEditor (\editor -> { editor | selection = Selection.Nothing }) model
                |> (\m -> { m | editHistory = EditHistory.markSavedPoint model.editor model.editHistory })
            , Interop.save (Editor.encode model.editor.email)
            )

        OpenPreview width ->
            ( model, Interop.openPreview width )

        Undo ->
            case EditHistory.undo model.editHistory of
                ( Just editor, editHistory ) ->
                    ( { model | editHistory = editHistory } |> mapEditor (\_ -> editor)
                    , Cmd.none
                    )

                ( Nothing, _ ) ->
                    ( model, Cmd.none )

        Redo ->
            case EditHistory.redo model.editHistory of
                ( Just editor, editHistory ) ->
                    ( { model | editHistory = editHistory } |> mapEditor (\_ -> editor)
                    , Cmd.none
                    )

                ( Nothing, _ ) ->
                    ( model, Cmd.none )

        -- ========================
        -- Drag and drop & Hovering
        -- ========================
        DragContentStarted name x y ->
            case Content.contentFromString name model.latestId of
                Just ( nextId, content ) ->
                    ( { model | drag = Dnd.startDraggingContent content x y, latestId = nextId, hover = Hovering.clear }, Cmd.none )

                Nothing ->
                    flashErrorString "Could not parse content string" model

        DragExistingContentStarted contentId x y ->
            case Editor.findContent contentId model.editor.email of
                Just content ->
                    ( { model | drag = Dnd.startDraggingContent content x y, hover = Hovering.clear }, Cmd.none )

                Nothing ->
                    flashErrorString "Could not find content by ID." model

        DragRowStarted name x y ->
            case Row.rowFromString name model.latestId of
                Just ( newId, row ) ->
                    ( { model | drag = Dnd.startDraggingRow row x y, latestId = newId, hover = Hovering.clear }, Cmd.none )

                Nothing ->
                    flashErrorString "Could not parse row string" model

        DragExistingRowStarted rowId x y ->
            case Editor.findRow rowId model.editor.email of
                Just row ->
                    ( { model | drag = Dnd.startDraggingRow row x y, hover = Hovering.clear }, Cmd.none )

                Nothing ->
                    flashErrorString "Could not find row by ID." model

        DragBlockResizerStarted rowId leftId rightId ->
            ( model
            , Browser.Dom.getElement (RowId.domId rowId)
                |> Task.attempt (GotElementBlockResizerRow rowId leftId rightId)
            )

        GotElementBlockResizerRow rowId leftId rightId result ->
            case result of
                Ok element ->
                    ( { model | drag = Dnd.ResizingBlocks rowId leftId rightId (DomElement.fromBrowser element) }
                    , Cmd.none
                    )

                Err err ->
                    flashErrorDom "Could not find row by ID." err model

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

                Dnd.ResizingBlocks _ _ _ _ ->
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

                Dnd.ResizingBlocks _ _ _ _ ->
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

                Dnd.ResizingBlocks _ _ _ _ ->
                    ( model1, Cmd.none )

                Dnd.NotDragging ->
                    ( model1, Cmd.none )

        GotElementMouseEnteredContent contentId result ->
            case result of
                Ok element ->
                    let
                        _ =
                            Debug.log "from the side" element
                    in
                    ( { model | drag = Dnd.mapContentTarget contentId (DomElement.fromBrowser element) model.drag }, Cmd.none )

                Err err ->
                    flashErrorDom "Could not find DOM element" err model

        MouseLeftContent ->
            ( { model | drag = Dnd.clearTargetContent model.drag, hover = Hovering.pop model.hover }, Cmd.none )

        DragMoved x y deltaX deltaY ->
            ( { model | drag = Dnd.mapCoordinate { x = x, y = y } model.drag }
                |> mapEditor (\editor -> { editor | email = Dnd.applyChanges { x = deltaX, y = deltaY } model.drag editor.email })
            , Cmd.none
            )

        DragStopped ->
            ( { model | drag = Dnd.NotDragging }
                |> track (\editor -> { editor | email = Dnd.drop editor.email model.drag })
            , Cmd.none
            )

        -- ========================
        -- Selection
        -- ========================
        SelectRow rowId ->
            ( track (\editor -> { editor | selection = Selection.selectRow rowId }) model, Cmd.none )

        SelectContent contentId ->
            ( track (\editor -> { editor | selection = Selection.selectContent contentId }) model, Cmd.none )

        ClearSelection ->
            ( track (\editor -> { editor | selection = Selection.Nothing }) model, Cmd.none )

        -- ========================
        -- Editing row
        -- ========================
        RemoveRow rowId ->
            ( track (\editor -> { editor | email = Editor.removeRow rowId editor.email }) model, Cmd.none )

        SetRowBackground rowId background ->
            mapRow rowId (Row.mapBackground background) model

        SetRowPadding rowId padding ->
            mapRow rowId (Row.mapPadding padding) model

        SetRowAlignment rowId alignment ->
            mapRow rowId (Row.mapAlignment alignment) model

        -- ========================
        -- Content: All
        -- ========================
        ContentRemoved contentId ->
            ( track (\editor -> { editor | email = Editor.removeContent contentId editor.email, selection = Selection.Nothing }) model
            , Cmd.none
            )

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

        SetImagePadding id padding ->
            mapImage id (Image.mapPadding padding) model

        TransitionImageToIllustrationPicker id ->
            mapImage id Image.mapTransitionToIllustrationPicker model

        CloseImageIllustrationsPicker id ->
            mapImage id Image.mapCloseIllustrationPicker model

        SetImageIllustrationsSearch id search ->
            mapImage id (Image.mapSearchIllustrations search) model

        SetImageIllustrationsColor id color ->
            mapImage id (Image.mapIllustrationColor color) model

        PickIllustrationForImage id base64 ->
            mapImage id (Image.mapPickIllustration base64) model

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

        SetDividerPadding id padding ->
            mapDivider id (Divider.mapPadding padding) model

        -- ========================
        -- Content: Text
        -- ========================
        SetTextTrix id trix ->
            mapText id (Text.mapTrix trix) model

        SetTextPadding id padding ->
            mapText id (Text.mapPadding padding) model

        -- ========================
        -- Content: Button
        -- ========================
        SetButtonTrix id trix ->
            mapButton id (Button.mapTrix trix) model

        SetButtonUrl id url ->
            mapButton id (Button.mapUrl url) model

        SetButtonPadding id padding ->
            mapButton id (Button.mapPadding padding) model


flashErrorDom : String -> Browser.Dom.Error -> Model -> ( Model, Cmd Msg )
flashErrorDom description error model =
    ( { model | error = Error.DomError description error }, Cmd.none )


flashErrorString : String -> Model -> ( Model, Cmd Msg )
flashErrorString description model =
    ( { model | error = Error.StringError description }, Cmd.none )


track : (Editor -> Editor) -> Model -> Model
track fn model =
    let
        newEditor =
            fn model.editor
    in
    { model | editor = newEditor, editHistory = EditHistory.push newEditor model.editHistory }


mapEditor : (Editor -> Editor) -> Model -> Model
mapEditor fn model =
    { model | editor = fn model.editor }


mapRow : RowId -> (Row -> Row) -> Model -> ( Model, Cmd Msg )
mapRow rowId fn model =
    ( mapEditor (\editor -> { editor | email = Editor.mapRow rowId fn editor.email }) model, Cmd.none )


mapImage : ContentId -> (Image.Image -> Image.Image) -> Model -> ( Model, Cmd Msg )
mapImage id fn model =
    ( mapEditor (\editor -> { editor | email = Editor.mapImage id fn editor.email }) model, Cmd.none )


mapDivider : ContentId -> (Divider.Divider -> Divider.Divider) -> Model -> ( Model, Cmd Msg )
mapDivider id fn model =
    ( mapEditor (\editor -> { editor | email = Editor.mapDivider id fn editor.email }) model, Cmd.none )


mapText : ContentId -> (Text.Text -> Text.Text) -> Model -> ( Model, Cmd Msg )
mapText id fn model =
    ( mapEditor (\editor -> { editor | email = Editor.mapText id fn editor.email }) model, Cmd.none )


mapButton : ContentId -> (Button.Button -> Button.Button) -> Model -> ( Model, Cmd Msg )
mapButton id fn model =
    ( mapEditor (\editor -> { editor | email = Editor.mapButton id fn editor.email }) model, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    let
        editorClass =
            if Dnd.isDragging model.drag then
                "ws-app is-dragging"

            else
                "ws-app"
    in
    div []
        [ viewToolbar model
        , div [ class editorClass ]
            [ viewEditor model
            , div [ class "ws-right-panel" ] [ viewRightPanel model ]
            , viewDraggingItem model.drag model.mouseOffsetX model.mouseOffsetY
            , viewDraggingTarget model.drag model.mouseOffsetX model.mouseOffsetY
            ]
        ]


viewToolbar : Model -> Html Msg
viewToolbar model =
    div [ class "ws-toolbar" ]
        [ div [ class "ws-container ws-flex ws-align-center ws-justify-between" ]
            [ div [ class "ws-toolbar-left" ]
                [ Dropdown.view
                    (div [ class "ws-toolbar-button" ] [ text "Preview..." ])
                    (div []
                        [ div [ onClick (OpenPreview "500px") ] [ text "Mobile" ]
                        , div [ onClick (OpenPreview "90%") ] [ text "Desktop" ]
                        ]
                    )
                , viewUndoButton model
                , viewRedoButton model
                ]
            , div [ class "ws-toolbar-right" ]
                [ div [ class "ws-toolbar-button", onClick Save ] [ Icon.download, text "Download" ]
                , viewSaveButton model
                ]
            ]
        ]


viewUndoButton : Model -> Html Msg
viewUndoButton model =
    if EditHistory.canUndo model.editHistory then
        div [ class "ws-toolbar-button", title "Undo", onClick Undo ] [ Icon.undo ]

    else
        div [ class "ws-toolbar-button ws-toolbar-button--disabled" ] [ Icon.undo ]


viewRedoButton : Model -> Html Msg
viewRedoButton model =
    if EditHistory.canRedo model.editHistory then
        div [ class "ws-toolbar-button", title "Redo", onClick Redo ] [ Icon.redo ]

    else
        div [ class "ws-toolbar-button ws-toolbar-button--disabled" ] [ Icon.redo ]


viewSaveButton : Model -> Html Msg
viewSaveButton model =
    if EditHistory.hasUnsavedChanges model.editHistory then
        div
            [ class "ws-toolbar-button ws-toolbar-button--highlight"
            , onClick Save
            , title "You've made changes - save it!"
            ]
            [ Icon.save, text "Save" ]

    else
        div [ class "ws-toolbar-button ws-toolbar-button--disabled", onClick Save ] [ Icon.save, text "Save" ]


viewEditor : Model -> Html Msg
viewEditor model =
    -- BUG: The onClick on the editor is clearing selection when resizing blocks.
    -- Maybe we should add a conditional here to not register onClick when the user
    -- is dragging something?
    div [ class "ws-editor ws-email theme-duo", onClick ClearSelection ]
        (List.map (viewEditorRow model) model.editor.email.rows)


viewEditorRow : Model -> Row -> Html Msg
viewEditorRow model row =
    let
        rowClick =
            if Dnd.isDragging model.drag then
                []

            else
                [ stopPropagationOn "click" (Decode.succeed ( SelectRow row.id, True )) ]
    in
    case row.layout of
        Row.Row100 block ->
            div
                ([ class "ws-row"
                 , id (RowId.domId row.id)
                 , onMouseEnter (MouseEnteredRow row.id)
                 , onMouseLeave MouseLeftRow
                 ]
                    ++ Padding.attributes row.padding
                    ++ rowClick
                    ++ [ Row.backgroundAttribute row ]
                )
                [ viewEditorRowControls model row
                , div [ class "ws-row-blocks" ]
                    [ div [ class "ws-block", style "width" "100%" ]
                        [ viewEditorBlock model block
                        ]
                    ]
                ]

        Row.Row50x50 left right ->
            div
                ([ class "ws-row"
                 , id (RowId.domId row.id)
                 , onMouseEnter (MouseEnteredRow row.id)
                 , onMouseLeave MouseLeftRow
                 ]
                    ++ Padding.attributes row.padding
                    ++ rowClick
                    ++ [ Row.backgroundAttribute row ]
                )
                [ viewEditorRowControls model row
                , div [ class "ws-row-blocks", Row.alignmentAttribute row ]
                    [ div [ class "ws-block", style "width" (String.fromFloat (left.width * 100) ++ "%") ]
                        [ viewEditorBlock model left
                        , viewEditorBlockResizer row.id left.id right.id
                        ]
                    , div [ class "ws-block", style "width" (String.fromFloat (right.width * 100) ++ "%") ]
                        [ viewEditorBlock model right
                        ]
                    ]
                ]

        Row.Row33x33x33 left center right ->
            div
                ([ class "ws-row"
                 , id (RowId.domId row.id)
                 , onMouseEnter (MouseEnteredRow row.id)
                 , onMouseLeave MouseLeftRow
                 ]
                    ++ Padding.attributes row.padding
                    ++ rowClick
                    ++ [ Row.backgroundAttribute row ]
                )
                [ viewEditorRowControls model row
                , div [ class "ws-row-blocks", Row.alignmentAttribute row ]
                    [ div [ class "ws-block", style "width" (String.fromFloat (left.width * 100) ++ "%") ]
                        [ viewEditorBlock model left
                        , viewEditorBlockResizer row.id left.id center.id
                        ]
                    , div [ class "ws-block", style "width" (String.fromFloat (center.width * 100) ++ "%") ]
                        [ viewEditorBlock model center
                        , viewEditorBlockResizer row.id center.id right.id
                        ]
                    , div [ class "ws-block", style "width" (String.fromFloat (right.width * 100) ++ "%") ]
                        [ viewEditorBlock model right
                        ]
                    ]
                ]


viewEditorRowControls : Model -> Row -> Html Msg
viewEditorRowControls model row =
    if Dnd.isDragging model.drag then
        text ""

    else if Selection.isRowSelected row.id model.editor.selection then
        div [ class "ws-row-hover ws-selected" ]
            [ div
                [ class "ws-move-icon"
                , Dnd.onMouseDown (DragExistingRowStarted row.id)
                ]
                [ Icon.move ]
            ]

    else if Hovering.isHoveringRow row.id model.hover then
        div [ class "ws-row-hover" ]
            [ div
                [ class "ws-move-icon"
                , Dnd.onMouseDown (DragExistingRowStarted row.id)
                ]
                [ Icon.move ]
            ]

    else
        text ""


viewEditorBlock : Model -> Block -> Html Msg
viewEditorBlock model block =
    if List.isEmpty block.contents then
        viewEmptyBlock model.drag block.id

    else
        Html.Keyed.node "div" [ id (BlockId.domId block.id) ] (List.map (viewEditorBlockContent model) block.contents)


viewEditorBlockResizer : RowId -> BlockId -> BlockId -> Html Msg
viewEditorBlockResizer rowId leftId rightId =
    div
        [ class "ws-block-resizer"
        , preventDefaultOn "mousedown" (Decode.succeed ( DragBlockResizerStarted rowId leftId rightId, True ))
        ]
        []


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


viewEditorBlockContent : Model -> Content -> ( String, Html Msg )
viewEditorBlockContent model content =
    let
        contentView =
            if Selection.isContentSelected (Content.contentId content) model.editor.selection then
                ( "selected-" ++ ContentId.domId (Content.contentId content), viewSelectedContent content )

            else
                ( "rendered-" ++ ContentId.domId (Content.contentId content), viewContent content )
    in
    ( ContentId.domId (Content.contentId content)
    , Html.Keyed.node "div"
        [ class "ws-content"
        , id (ContentId.domId (Content.contentId content))
        , onMouseEnter (MouseEnteredContent (Content.contentId content))
        , onMouseLeave MouseLeftContent
        , stopPropagationOn "click" (Decode.succeed ( SelectContent (Content.contentId content), True ))
        ]
        [ ( "controls-" ++ ContentId.domId (Content.contentId content)
          , viewEditorBlockContentControls model content
          )
        , contentView
        ]
    )


viewContent : Content -> Html Msg
viewContent content =
    case content of
        Content.Button id button ->
            div
                ([ class "ws-button-container"
                 , attribute "data-url" button.url
                 ]
                    ++ Padding.attributes button.padding
                )
                [ div [ class "ws-button" ]
                    [ RawHtml.view (Trix.html button.trix)
                    ]
                ]

        Content.Divider id divider ->
            let
                strokeStyle =
                    case divider.stroke of
                        Divider.Solid ->
                            "solid"

                        Divider.Dashed ->
                            "dashed"

                        Divider.Dotted ->
                            "dotted"
            in
            div (Padding.attributes divider.padding)
                [ div
                    [ style "width" (String.fromInt divider.widthPercentage ++ "%")
                    , style "border-top" (String.fromInt divider.thickness ++ "px " ++ strokeStyle ++ " " ++ divider.color)
                    ]
                    []
                ]

        Content.Image id image ->
            Image.render image

        Content.Text id editingText ->
            div (Padding.attributes editingText.padding)
                [ RawHtml.view (Trix.html editingText.trix)
                ]

        Content.YoutubeVideo id ->
            text "Render youtube video"


viewSelectedContent : Content -> Html Msg
viewSelectedContent content =
    case content of
        Content.Button id button ->
            div (class "ws-button-container" :: Padding.attributes button.padding)
                [ div [ class "ws-button" ]
                    [ Trix.editor (SetButtonTrix id) button.trix
                    ]
                ]

        Content.Divider id divider ->
            viewContent content

        Content.Image _ image ->
            Image.render image

        Content.Text id text ->
            div (Padding.attributes text.padding)
                [ Trix.editor (SetTextTrix id) text.trix
                ]

        Content.YoutubeVideo id ->
            viewContent content


viewEditorBlockContentControls : Model -> Content -> Html Msg
viewEditorBlockContentControls model content =
    if Dnd.isDragging model.drag then
        text ""

    else if Selection.isContentSelected (Content.contentId content) model.editor.selection then
        div [ class "ws-content-hover ws-selected" ]
            [ div
                [ class "ws-move-icon"
                , Dnd.onMouseDown (DragExistingContentStarted (Content.contentId content))
                ]
                [ Icon.move ]
            ]

    else if Hovering.isHoveringContent (Content.contentId content) model.hover then
        div [ class "ws-content-hover" ]
            [ div
                [ class "ws-move-icon"
                , Dnd.onMouseDown (DragExistingContentStarted (Content.contentId content))
                ]
                [ Icon.move ]
            ]

    else
        text ""


viewRightPanel : Model -> Html Msg
viewRightPanel model =
    case model.editor.selection of
        Selection.Nothing ->
            viewDraggableItems model

        Selection.RowSelected rowId ->
            case Editor.findRow rowId model.editor.email of
                Just row ->
                    Row.editor
                        { row = row
                        , remove = RemoveRow rowId
                        , close = ClearSelection
                        , setBackground = SetRowBackground rowId
                        , setPadding = SetRowPadding rowId
                        , setAlignment = SetRowAlignment rowId
                        }

                Nothing ->
                    text ""

        Selection.ContentSelected contentId ->
            case Editor.findContent contentId model.editor.email of
                Just content ->
                    case content of
                        Content.Button id button ->
                            Button.editor
                                { button = button
                                , remove = ContentRemoved id
                                , close = ClearSelection
                                , setUrl = SetButtonUrl id
                                , setPadding = SetButtonPadding id
                                }

                        Content.Divider id divider ->
                            Divider.editor
                                { divider = divider
                                , remove = ContentRemoved id
                                , close = ClearSelection
                                , setWidthPercentage = SetDividerWidthPercentage id
                                , setStroke = SetDividerStroke id
                                , setThickness = SetDividerThickness id
                                , setColor = SetDividerColor id
                                , setPadding = SetDividerPadding id
                                }

                        Content.Image id image ->
                            Image.editor
                                { image = image
                                , contentId = id
                                , illustrations = model.illustrations
                                , remove = ContentRemoved id
                                , close = ClearSelection
                                , setUrl = SetImageUrl id
                                , setWidth = SetImageWidth id
                                , setAlignment = SetImageAlignment id
                                , setAltDescription = SetImageAltDescription id
                                , setActionUrl = SetImageActionUrl id
                                , setPadding = SetImagePadding id
                                , startPickingIllustrations = TransitionImageToIllustrationPicker id
                                , stopPickingIllustrations = CloseImageIllustrationsPicker id
                                , setIllustrationsSearch = SetImageIllustrationsSearch id
                                , setIllustrationsColor = SetImageIllustrationsColor id
                                , pickIllustration = PickIllustrationForImage id
                                }

                        Content.Text id editingText ->
                            Text.editor
                                { text = editingText
                                , remove = ContentRemoved id
                                , close = ClearSelection
                                , setPadding = SetTextPadding id
                                }

                        Content.YoutubeVideo id ->
                            text "Youtube video"

                Nothing ->
                    text "??"


viewDraggableItems : Model -> Html Msg
viewDraggableItems model =
    div [ class "ws-draggable-items" ]
        [ viewDraggableContent model "Text" [ Icon.text, text Lang.text ]
        , viewDraggableContent model "Image" [ Icon.image, text Lang.image ]
        , viewDraggableContent model "Button" [ Icon.button, text Lang.button ]
        , viewDraggableContent model "Divider" [ Icon.divider, text Lang.divider ]

        -- , viewDraggableContent model "YoutubeVideo" [ Icon.youtube, text Lang.youtubeVideo ]
        , UI.divider
        , viewDraggableRow model
            "Row100"
            [ div [ class "ws-draggable-row-column ws-bg-gray-400", style "width" "100%" ] []
            ]
        , viewDraggableRow model
            "Row50x50"
            [ div [ class "ws-draggable-row-column ws-bg-gray-400", style "width" "50%" ] []
            , div [ class "ws-draggable-row-column ws-bg-gray-400", style "width" "50%" ] []
            ]
        , viewDraggableRow model
            "Row33x33x33"
            [ div [ class "ws-draggable-row-column ws-bg-gray-400", style "width" "33%" ] []
            , div [ class "ws-draggable-row-column ws-bg-gray-400", style "width" "33%" ] []
            , div [ class "ws-draggable-row-column ws-bg-gray-400", style "width" "33%" ] []
            ]
        ]


viewDraggableContent : Model -> String -> List (Html Msg) -> Html Msg
viewDraggableContent model contentName children =
    div
        [ class "ws-draggable-item"
        , Dnd.onMouseDown (DragContentStarted contentName)
        ]
        children


viewDraggableRow : Model -> String -> List (Html Msg) -> Html Msg
viewDraggableRow model rowName children =
    div
        [ class "ws-draggable-row"
        , Dnd.onMouseDown (DragRowStarted rowName)
        ]
        children


{-| Renders the item being dragged, if there is one. The item os absolute
positioned based on the dragging coordinate, so it doesn't matter where
in the DOM tree this function is called.
-}
viewDraggingItem : Dnd.DragState -> Int -> Int -> Html Msg
viewDraggingItem drag offsetX offsetY =
    case drag of
        Dnd.DraggingRow row coordinate target ->
            div
                [ class "ws-item-being-dragged"
                , style "top" (String.fromInt (coordinate.y + offsetY) ++ "px")
                , style "left" (String.fromInt (coordinate.x + offsetX) ++ "px")
                ]
                [ text "Block" ]

        Dnd.DraggingContent item coordinate target ->
            div
                [ class "ws-item-being-dragged"
                , style "top" (String.fromInt (coordinate.y + offsetY) ++ "px")
                , style "left" (String.fromInt (coordinate.x + offsetX) ++ "px")
                ]
                [ text "Content" ]

        Dnd.ResizingBlocks _ _ _ _ ->
            text ""

        Dnd.NotDragging ->
            text ""


viewDraggingTarget : Dnd.DragState -> Int -> Int -> Html Msg
viewDraggingTarget dragState offsetX offsetY =
    case dragState of
        Dnd.DraggingRow item xy target ->
            case target of
                Dnd.AfterRow _ element ->
                    div
                        [ class "ws-drop-target"
                        , style "top" (String.fromInt (element.y + offsetY + element.height) ++ "px")
                        , style "left" (String.fromInt (element.x + offsetX) ++ "px")
                        , style "width" (String.fromInt element.width ++ "px")
                        ]
                        [ div [ class "ws-drop-target-text" ] [ text "Drop here" ] ]

                Dnd.BeforeRow row element ->
                    div
                        [ class "ws-drop-target"
                        , style "top" (String.fromInt (element.y + offsetY) ++ "px")
                        , style "left" (String.fromInt (element.x + offsetX) ++ "px")
                        , style "width" (String.fromInt element.width ++ "px")
                        ]
                        [ div [ class "ws-drop-target-text" ] [ text "Drop here" ] ]

                Dnd.NoTargetButHoveringRow _ _ ->
                    text ""

                Dnd.NoTargetRow ->
                    text ""

        Dnd.DraggingContent item xy target ->
            case target of
                Dnd.AfterContent _ element ->
                    div
                        [ class "ws-drop-target"
                        , style "top" (String.fromInt (element.y + offsetY + element.height) ++ "px")
                        , style "left" (String.fromInt (element.x + offsetX) ++ "px")
                        , style "width" (String.fromInt element.width ++ "px")
                        ]
                        [ div [ class "ws-drop-target-text" ] [ text "Drop here" ] ]

                Dnd.BeforeContent row element ->
                    div
                        [ class "ws-drop-target"
                        , style "top" (String.fromInt (element.y + offsetY) ++ "px")
                        , style "left" (String.fromInt (element.x + offsetX) ++ "px")
                        , style "width" (String.fromInt element.width ++ "px")
                        ]
                        [ div [ class "ws-drop-target-text" ] [ text "Drop here" ] ]

                Dnd.FirstOfBlock _ ->
                    text ""

                Dnd.NoTargetContent ->
                    text ""

                Dnd.NoTargetButHoveringContent _ _ ->
                    text ""

        Dnd.ResizingBlocks _ _ _ _ ->
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
                    Decode.map4 DragMoved
                        (Decode.field "pageX" Decode.int)
                        (Decode.field "pageY" Decode.int)
                        (Decode.field "movementX" Decode.int)
                        (Decode.field "movementY" Decode.int)

            else
                Sub.none

        mouseUpForDragging =
            if Dnd.isDragging model.drag then
                Browser.Events.onMouseUp (Decode.succeed DragStopped)

            else
                Sub.none
    in
    Sub.batch
        [ Interop.load Load
        , mouseMoveForDragging
        , mouseUpForDragging
        , Interop.uploadProgress SetImageUploadProgress
        , Interop.uploadDone SetImageUploadDone
        , Interop.illustrations GotIllustrations
        , Interop.getContents (\_ -> EncodeAndSendContents)
        , Interop.ctrlZ (\_ -> Undo)
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
