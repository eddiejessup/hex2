module Hex.Stage.Build.Horizontal.Paragraph.Break where

import Formatting qualified as F
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.AnyDirection.Breaking.Badness qualified as Bad
import Hex.Stage.Build.AnyDirection.Breaking.Types qualified as Page
import Hex.Stage.Build.AnyDirection.Evaluate qualified as Page
import Hex.Stage.Build.Horizontal.Evaluate
import Hex.Stage.Build.Horizontal.Paragraph.Types
import Hex.Stage.Build.ListElem qualified as ListElem
import Hexlude

data ChunkedHListElem
  = HListChunk (Seq ListElem.HListElem)
  | HListBreakItem ListElem.HListElem Page.BreakItem
  deriving stock (Show, Generic)

fmtChunkedHListElem :: Fmt ChunkedHListElem
fmtChunkedHListElem = F.later $ \case
  HListChunk els -> bformat (F.commaSpaceSep ListElem.fmtHListElem) els
  HListBreakItem _ b -> bformat ("Break: " |%| F.shown) b

chunkHList :: ListElem.HList -> Seq ChunkedHListElem
chunkHList hList = foldl' f Empty (withBreaks hList)
  where
    f fState (x, mayBreak) = case mayBreak of
      Nothing -> case fState of
        chunkedPre :|> HListChunk xs -> chunkedPre :|> HListChunk (xs :|> x)
        _ -> fState :|> HListChunk (Empty :|> x)
      Just b -> fState :|> HListBreakItem x b

fmtLine :: Fmt (Seq ChunkedHListElem)
fmtLine = F.unlined fmtChunkedHListElem

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

finaliseHList :: ListElem.HList -> ListElem.HList
finaliseHList (ListElem.HList Empty) =
  ListElem.HList mempty
finaliseHList (ListElem.HList elems@(elemInit :|> lastElem)) =
  let -- Remove the final item if it's glue.
      trimmedElems = case lastElem of
        ListElem.HVListElem (ListElem.ListGlue _) -> elemInit
        _ -> elems
      -- Add extra bits to finish the list.
      finishedElems = trimmedElems >< finishingElems
   in ListElem.HList finishedElems
  where
    -- \penalty10k \hfil \penalty-10k.
    finishingElems :: Seq ListElem.HListElem
    finishingElems =
      Empty
        :|> ListElem.HVListElem (ListElem.ListPenalty $ ListElem.Penalty $ Q.HexInt Q.tenK)
        :|> ListElem.HVListElem (ListElem.ListGlue (Q.filStretchGlue Q.bigFilLength))
        :|> ListElem.HVListElem (ListElem.ListPenalty $ ListElem.Penalty $ Q.HexInt $ -Q.tenK)

newtype Line = Line {unLine :: Seq ListElem.HListElem}
  deriving stock (Show, Generic)

newtype LineSequence = LineSequence {unLineSequence :: Seq Line}
  deriving stock (Show, Generic)

-- allBreakSequences :: Seq HListElem -> Seq LineSequence
-- allBreakSequences = go (Empty, Empty)
--   where
--     go :: (LineSequence, Line) -> Seq HListElem -> Seq LineSequence
--     go (lineSequence, lineAccum) = \case
--       -- If nothing remains, add the final line to the break sequence.
--       Empty -> singleton (lineSequence :|> lineAccum)
--       e@(HListChunk _) :<| xs ->
--         go (lineSequence, lineAccum :|> e) xs
--       e@(HListBreakItem _ _) :<| xs ->
--         let -- If we break here.
--             laterBreakSequencesYesBreak = go (Empty :|> lineAccum, Empty) xs

--             -- If we don't break here.
--             laterBreakSequencesNoBreak = go (Empty, lineAccum :|> e) xs

--             laterBreakSequencesAll = laterBreakSequencesYesBreak >< laterBreakSequencesNoBreak

--             breakSequences = laterBreakSequencesAll <&> \laterSeq -> lineSequence >< laterSeq
--          in breakSequences

-- breakParagraph ::
--   (Monad m) =>
--   Q.Length -> -- HSize
--   Q.HexInt -> -- Tolerance
--   Q.HexInt -> -- LinePenalty
--   ListElem.HList ->
--   m (Seq (BoxElem.Box BoxElem.HBox))
-- breakParagraph _ _ _ (ListElem.HList Empty) =
--   pure mempty
-- breakParagraph dw tol lp hList = do
--   let elemsWithBreaks = finaliseHList hList
--   panic "not implemented"

data DiscardingState = Discarding | NotDiscarding

withBreaks :: ListElem.HList -> Seq (ListElem.HListElem, Maybe Page.BreakItem)
withBreaks = seqOf (#unHList % to toAdjacents % folded % to (\adj@(_, e, _) -> (e, hListElemToBreakItem adj)))

breakGreedy ::
  Q.Length -> -- HSize
  Q.HexInt -> -- Tolerance
  Q.HexInt -> -- LinePenalty
  ListElem.HList ->
  Seq ListElem.HList
breakGreedy dw _tol _lp (ListElem.HList allEs) =
  let finalisedHList = finaliseHList (ListElem.HList allEs)

      eWBs :: Seq (ListElem.HListElem, Maybe Page.BreakItem)
      eWBs = withBreaks finalisedHList

      lineSeq = go (LineSequence Empty, Line Empty, NotDiscarding) eWBs
   in -- Map from the line breaking types to our result type.
      seqOf (#unLineSequence % folded % #unLine % to ListElem.HList) lineSeq
  where
    go :: (LineSequence, Line, DiscardingState) -> Seq (ListElem.HListElem, Maybe Page.BreakItem) -> LineSequence
    go st@(lnSeq@(LineSequence lns), ln@(Line lnEs), discarding) = \case
      Empty -> LineSequence (lns :|> ln)
      aEs@((e, mayBI) :<| rEs) ->
        case discarding of
          Discarding
            | hListElemisDiscardable e ->
                go st rEs
          _ ->
            case mayBI of
              Nothing ->
                go (lnSeq, Line (lnEs :|> e), NotDiscarding) rEs
              Just bI ->
                let spec = listFlexSpec (ListElem.HList lnEs) dw
                    b = Page.glueFlexSpecBadness spec
                 in case b of
                      Bad.InfiniteBadness ->
                        let newLnSeq = lns :|> ln
                            newEs = case bI of
                              Page.GlueBreak _ -> aEs
                              Page.KernBreak _ -> aEs
                              Page.PenaltyBreak _ -> rEs
                         in go (LineSequence newLnSeq, Line Empty, Discarding) newEs
                      Bad.FiniteBadness _finiteBadness ->
                        go (lnSeq, Line (lnEs :|> e), NotDiscarding) rEs
