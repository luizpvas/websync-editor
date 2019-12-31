module Editor exposing
    ( Block
    , Content(..)
    , Email
    , Row(..)
    , addContentAfterAnotherContent
    , addContentBeforeAnotherContent
    , addFirstContentToBlock
    , addRowAfterAnotherRow
    , addRowBeforeAnotherRow
    , contentFromString
    , contentId
    , emptyEmail
    , findContent
    , findRow
    , initWithRows
    , mapContents
    , mapDivider
    , mapImage
    , mapText
    , removeContent
    , removeRow
    , rowFromString
    , rowId
    )

-- Email

import BlockId exposing (BlockId)
import Content.Divider as Divider
import Content.Image as Image
import Content.Text as Text
import ContentId exposing (ContentId)
import List.Extra
import RowId exposing (RowId)


type alias Email =
    { rows : List Row
    }


{-| The empty email starts with a block because the UI and
Drag and Drop breaks without at least one element in the
screen.
-}
emptyEmail : Email
emptyEmail =
    { rows = [ Row100 (RowId.fromInt 1) { id = BlockId.fromInt 1, contents = [] } ]
    }


{-| Initializes an email with the given rows. This function is used in tests.
-}
initWithRows : List Row -> Email
initWithRows rows =
    { rows = rows }


findRow : RowId -> Email -> Maybe Row
findRow id email =
    email.rows |> List.filter (\row -> rowId row == id) |> List.head


findContent : ContentId -> Email -> Maybe Content
findContent id email =
    email
        |> flattenBlocks
        |> List.filterMap
            (\block ->
                block.contents
                    |> List.filter (\content -> contentId content == id)
                    |> List.head
            )
        |> List.head


flattenBlocks : Email -> List Block
flattenBlocks email =
    email.rows
        |> List.concatMap
            (\row ->
                case row of
                    Row100 _ block ->
                        [ block ]

                    Row50x50 _ left right ->
                        [ left, right ]

                    Row33x33x33 _ left middle right ->
                        [ left, middle, right ]
            )


addRowAfterAnotherRow : Row -> RowId -> Email -> Email
addRowAfterAnotherRow toBeAdded afterThis email =
    if Maybe.map rowId (List.Extra.last email.rows) == Just afterThis then
        { email | rows = email.rows ++ [ toBeAdded ] }

    else
        List.Extra.findIndex (\row -> rowId row == afterThis) email.rows
            |> Maybe.andThen (\index -> List.Extra.getAt (index + 1) email.rows)
            |> Maybe.map (\beforeRow -> addRowBeforeAnotherRow toBeAdded (rowId beforeRow) email)
            |> Maybe.withDefault email


addRowBeforeAnotherRow : Row -> RowId -> Email -> Email
addRowBeforeAnotherRow toBeAdded beforeThis email =
    let
        newRows =
            List.Extra.takeWhile (\row -> rowId row /= beforeThis) email.rows
                ++ [ toBeAdded ]
                ++ List.Extra.dropWhile (\row -> rowId row /= beforeThis) email.rows
    in
    { email | rows = newRows }


addFirstContentToBlock : Content -> BlockId -> Email -> Email
addFirstContentToBlock content blockId =
    mapBlocks
        (\block ->
            if block.id == blockId then
                { block | contents = [ content ] }

            else
                block
        )


addContentBeforeAnotherContent : Content -> ContentId -> Email -> Email
addContentBeforeAnotherContent toBeAdded beforeThis =
    mapBlocks
        (\block ->
            case List.Extra.find (\c -> contentId c == beforeThis) block.contents of
                Just _ ->
                    { block
                        | contents =
                            List.Extra.takeWhile (\c -> contentId c /= beforeThis) block.contents
                                ++ [ toBeAdded ]
                                ++ List.Extra.dropWhile (\c -> contentId c /= beforeThis) block.contents
                    }

                Nothing ->
                    block
        )


addContentAfterAnotherContent : Content -> ContentId -> Email -> Email
addContentAfterAnotherContent toBeAdded afterThis =
    mapBlocks
        (\block ->
            case List.Extra.find (\c -> contentId c == afterThis) block.contents of
                Just _ ->
                    { block
                        | contents =
                            List.Extra.dropWhileRight (\c -> contentId c /= afterThis) block.contents
                                ++ [ toBeAdded ]
                                ++ List.Extra.takeWhileRight (\c -> contentId c /= afterThis) block.contents
                    }

                Nothing ->
                    block
        )


removeRow : RowId -> Email -> Email
removeRow toBeRemovedId email =
    { email | rows = email.rows |> List.filter (\row -> rowId row /= toBeRemovedId) }


removeContent : ContentId -> Email -> Email
removeContent toBeRemovedId =
    mapBlocks
        (\block ->
            { block | contents = block.contents |> List.filter (\c -> contentId c /= toBeRemovedId) }
        )


mapRows : (Row -> Row) -> Email -> Email
mapRows fn email =
    { email | rows = email.rows |> List.map fn }


mapBlocks : (Block -> Block) -> Email -> Email
mapBlocks fn =
    mapRows
        (\row ->
            case row of
                Row100 id block ->
                    Row100 id (fn block)

                Row50x50 id left right ->
                    Row50x50 id (fn left) (fn right)

                Row33x33x33 id left middle right ->
                    Row33x33x33 id (fn left) (fn middle) (fn right)
        )


mapContents : (Content -> Content) -> Email -> Email
mapContents fn =
    mapBlocks (\block -> { block | contents = block.contents |> List.map fn })


mapImage : ContentId -> (Image.Image -> Image.Image) -> Email -> Email
mapImage targetId fn =
    mapContents
        (\content ->
            case content of
                Image imageId image ->
                    if imageId == targetId then
                        Image imageId (fn image)

                    else
                        content

                _ ->
                    content
        )


mapDivider : ContentId -> (Divider.Divider -> Divider.Divider) -> Email -> Email
mapDivider targetId fn =
    mapContents
        (\content ->
            case content of
                Divider dividerId divider ->
                    if dividerId == targetId then
                        Divider dividerId (fn divider)

                    else
                        content

                _ ->
                    content
        )


mapText : ContentId -> (Text.Text -> Text.Text) -> Email -> Email
mapText targetId fn =
    mapContents
        (\content ->
            case content of
                Text textId text ->
                    if textId == targetId then
                        Text textId (fn text)

                    else
                        content

                _ ->
                    content
        )



-- Block


type Row
    = Row100 RowId Block
    | Row50x50 RowId Block Block
    | Row33x33x33 RowId Block Block Block


type alias Block =
    { id : BlockId
    , contents : ContentList
    }


rowId : Row -> RowId
rowId row =
    case row of
        Row100 id _ ->
            id

        Row50x50 id _ _ ->
            id

        Row33x33x33 id _ _ _ ->
            id


rowFromString : String -> Int -> Maybe ( Int, Row )
rowFromString rowName latestId =
    let
        newBlock =
            \id row ->
                row { id = BlockId.fromInt id, contents = [] }
    in
    case rowName of
        "Row100" ->
            Just
                ( latestId + 2
                , Row100 (RowId.fromInt latestId)
                    |> newBlock (latestId + 1)
                )

        "Row50x50" ->
            Just
                ( latestId + 3
                , Row50x50 (RowId.fromInt latestId)
                    |> newBlock (latestId + 1)
                    |> newBlock (latestId + 2)
                )

        "Row33x33x33" ->
            Just
                ( latestId + 4
                , Row33x33x33 (RowId.fromInt latestId)
                    |> newBlock (latestId + 1)
                    |> newBlock (latestId + 2)
                    |> newBlock (latestId + 3)
                )

        _ ->
            Nothing



-- Content


type alias ContentList =
    List Content


type Content
    = Button ContentId
    | Divider ContentId Divider.Divider
    | Image ContentId Image.Image
    | Text ContentId Text.Text
    | YoutubeVideo ContentId


contentId : Content -> ContentId
contentId content =
    case content of
        Button id ->
            id

        Divider id _ ->
            id

        Image id _ ->
            id

        Text id _ ->
            id

        YoutubeVideo id ->
            id


contentFromString : String -> Int -> Maybe ( Int, Content )
contentFromString name id =
    case name of
        "Button" ->
            Just ( id + 1, Button (ContentId.fromInt id) )

        "Divider" ->
            Just ( id + 1, Divider (ContentId.fromInt id) Divider.default )

        "Image" ->
            Just ( id + 1, Image (ContentId.fromInt id) Image.default )

        "Text" ->
            Just ( id + 1, Text (ContentId.fromInt id) Text.default )

        "YoutubeVideo" ->
            Just ( id + 1, YoutubeVideo (ContentId.fromInt id) )

        _ ->
            Nothing
