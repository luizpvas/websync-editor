module Editor exposing
    ( Email
    , addContentAfterAnotherContent
    , addContentBeforeAnotherContent
    , addFirstContentToBlock
    , addRowAfterAnotherRow
    , addRowBeforeAnotherRow
    , decoder
    , emptyEmail
    , encode
    , findContent
    , findRow
    , initWithRows
    , mapButton
    , mapContents
    , mapDivider
    , mapImage
    , mapRow
    , mapText
    , removeContent
    , removeRow
    , resizeBlocks
    )

-- Email

import Block exposing (Block)
import BlockId exposing (BlockId)
import Colorpicker
import Content
import Content.Button as Button
import Content.Divider as Divider
import Content.Image as Image
import Content.Text as Text
import ContentId exposing (ContentId)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra
import Padding
import Row exposing (Row)
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
    { rows =
        [ { id = RowId.fromInt 1
          , background = Colorpicker.white
          , padding = Padding.default 0
          , alignment = Row.Start
          , layout = Row.Row100 { id = BlockId.fromInt 1, width = 1.0, contents = [] }
          }
        ]
    }


{-| Initializes an email with the given rows. This function is used in tests.
-}
initWithRows : List Row -> Email
initWithRows rows =
    { rows = rows }


findRow : RowId -> Email -> Maybe Row
findRow id email =
    email.rows |> List.filter (\row -> row.id == id) |> List.head


findContent : ContentId -> Email -> Maybe Content.Content
findContent id email =
    email
        |> flattenBlocks
        |> List.filterMap
            (\block ->
                block.contents
                    |> List.filter (\content -> Content.contentId content == id)
                    |> List.head
            )
        |> List.head


flattenBlocks : Email -> List Block
flattenBlocks email =
    email.rows
        |> List.concatMap
            (\row ->
                case row.layout of
                    Row.Row100 block ->
                        [ block ]

                    Row.Row50x50 left right ->
                        [ left, right ]

                    Row.Row33x33x33 left middle right ->
                        [ left, middle, right ]
            )


addRowAfterAnotherRow : Row -> RowId -> Email -> Email
addRowAfterAnotherRow toBeAdded afterThis email =
    if Maybe.map .id (List.Extra.last email.rows) == Just afterThis then
        { email | rows = email.rows ++ [ toBeAdded ] }

    else
        List.Extra.findIndex (\row -> row.id == afterThis) email.rows
            |> Maybe.andThen (\index -> List.Extra.getAt (index + 1) email.rows)
            |> Maybe.map (\beforeRow -> addRowBeforeAnotherRow toBeAdded beforeRow.id email)
            |> Maybe.withDefault email


addRowBeforeAnotherRow : Row -> RowId -> Email -> Email
addRowBeforeAnotherRow toBeAdded beforeThis email =
    let
        newRows =
            List.Extra.takeWhile (\row -> row.id /= beforeThis) email.rows
                ++ toBeAdded
                :: List.Extra.dropWhile (\row -> row.id /= beforeThis) email.rows
    in
    { email | rows = newRows }


addFirstContentToBlock : Content.Content -> BlockId -> Email -> Email
addFirstContentToBlock content blockId =
    mapBlocks
        (\block ->
            if block.id == blockId then
                { block | contents = [ content ] }

            else
                block
        )


addContentBeforeAnotherContent : Content.Content -> ContentId -> Email -> Email
addContentBeforeAnotherContent toBeAdded beforeThis =
    mapBlocks
        (\block ->
            case List.Extra.find (\c -> Content.contentId c == beforeThis) block.contents of
                Just _ ->
                    { block
                        | contents =
                            List.Extra.takeWhile (\c -> Content.contentId c /= beforeThis) block.contents
                                ++ toBeAdded
                                :: List.Extra.dropWhile (\c -> Content.contentId c /= beforeThis) block.contents
                    }

                Nothing ->
                    block
        )


addContentAfterAnotherContent : Content.Content -> ContentId -> Email -> Email
addContentAfterAnotherContent toBeAdded afterThis =
    mapBlocks
        (\block ->
            case List.Extra.find (\c -> Content.contentId c == afterThis) block.contents of
                Just _ ->
                    { block
                        | contents =
                            List.Extra.dropWhileRight (\c -> Content.contentId c /= afterThis) block.contents
                                ++ toBeAdded
                                :: List.Extra.takeWhileRight (\c -> Content.contentId c /= afterThis) block.contents
                    }

                Nothing ->
                    block
        )


removeRow : RowId -> Email -> Email
removeRow toBeRemovedId email =
    { email | rows = email.rows |> List.filter (\row -> row.id /= toBeRemovedId) }


removeContent : ContentId -> Email -> Email
removeContent toBeRemovedId =
    mapBlocks
        (\block ->
            { block | contents = block.contents |> List.filter (\c -> Content.contentId c /= toBeRemovedId) }
        )


mapRows : (Row -> Row) -> Email -> Email
mapRows fn email =
    { email | rows = email.rows |> List.map fn }


mapBlocks : (Block -> Block) -> Email -> Email
mapBlocks fn =
    mapRows
        (\row ->
            case row.layout of
                Row.Row100 block ->
                    { row | layout = Row.Row100 (fn block) }

                Row.Row50x50 left right ->
                    { row | layout = Row.Row50x50 (fn left) (fn right) }

                Row.Row33x33x33 left middle right ->
                    { row | layout = Row.Row33x33x33 (fn left) (fn middle) (fn right) }
        )


mapRow : RowId -> (Row -> Row) -> Email -> Email
mapRow rowId fn =
    mapRows
        (\row ->
            if row.id == rowId then
                fn row

            else
                row
        )


resizeBlocks : BlockId -> BlockId -> Float -> Email -> Email
resizeBlocks leftId rightId delta =
    mapBlocks
        (\block ->
            if block.id == leftId then
                { block | width = block.width + delta }

            else if block.id == rightId then
                { block | width = block.width - delta }

            else
                block
        )


mapContents : (Content.Content -> Content.Content) -> Email -> Email
mapContents fn =
    mapBlocks (\block -> { block | contents = block.contents |> List.map fn })


mapImage : ContentId -> (Image.Image -> Image.Image) -> Email -> Email
mapImage targetId fn =
    mapContents
        (\content ->
            case content of
                Content.Image imageId image ->
                    if imageId == targetId then
                        Content.Image imageId (fn image)

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
                Content.Divider dividerId divider ->
                    if dividerId == targetId then
                        Content.Divider dividerId (fn divider)

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
                Content.Text textId text ->
                    if textId == targetId then
                        Content.Text textId (fn text)

                    else
                        content

                _ ->
                    content
        )


mapButton : ContentId -> (Button.Button -> Button.Button) -> Email -> Email
mapButton targetId fn =
    mapContents
        (\content ->
            case content of
                Content.Button buttonId button ->
                    if buttonId == targetId then
                        Content.Button buttonId (fn button)

                    else
                        content

                _ ->
                    content
        )



-- Json


encode : Email -> Value
encode email =
    Encode.object
        [ ( "rows", Encode.list Row.encode email.rows )
        ]


decoder : Decoder Email
decoder =
    Decode.map Email
        (Decode.field "rows" (Decode.list Row.decoder))
