module Hex.Stage.Build.Horizontal.Paragraph.Break.Chunk where

import Formatting qualified as F
import Hex.Stage.Build.Horizontal.Paragraph.Types
import Hex.Stage.Build.ListElem (HListElem)
import Hex.Stage.Build.ListElem qualified as ListElem
import Hexlude

data ChunkedHListItem
  = Chunk (Seq HListElem)
  | ChunkedBreakItem HBreakItem
  deriving stock (Show, Generic)

data AdjState a
  = AtFirstElem
  | PastFirstElem a (Seq (Maybe a, a, Maybe a))

toAdjacents :: Foldable t => t a -> Seq (Maybe a, a, Maybe a)
toAdjacents xs = case foldl' f AtFirstElem xs of
  AtFirstElem -> Empty
  PastFirstElem onlyElem Empty -> singleton (Nothing, onlyElem, Nothing)
  PastFirstElem finalElem acc@(_ :|> (_, left, _)) -> acc :|> (Just left, finalElem, Nothing)
  where
    f fState right =
      PastFirstElem right $ case fState of
        AtFirstElem -> Empty
        PastFirstElem v acc ->
          let maybeLeft = case acc of
                Empty -> Nothing
                (_ :|> (_, left, _)) -> Just left
           in acc :|> (maybeLeft, v, Just right)

withBreaks :: Foldable t => Fold (t ListElem.HListElem) (HListElem, Maybe HBreakItem)
withBreaks =
  to toAdjacents
    % folded
    % to (\adj@(_, e, _) -> (e, hListElemToBreakItem adj))

fmtChunkedListItem :: Fmt ChunkedHListItem
fmtChunkedListItem = F.later $ \case
  Chunk els -> bformat (F.commaSpaceSep ListElem.fmtHListElem) els
  ChunkedBreakItem b -> bformat ("Break: " |%| F.shown) b

asChunkedList :: Seq ListElem.HListElem -> Seq ChunkedHListItem
asChunkedList = foldlOf' withBreaks f Empty
  where
    f fState (x, mayBreak) = case mayBreak of
      Nothing -> case fState of
        chunkedPre :|> Chunk xs -> chunkedPre :|> Chunk (xs :|> x)
        _ -> fState :|> Chunk (Empty :|> x)
      Just b -> fState :|> ChunkedBreakItem b

chunkedListItemElems :: ChunkedHListItem -> Seq HListElem
chunkedListItemElems = \case
  Chunk xs -> xs
  ChunkedBreakItem x -> hBreakItemAsListElemsNoBreak x

fmtLine :: Fmt (Seq ChunkedHListItem)
fmtLine = F.unlined fmtChunkedListItem
