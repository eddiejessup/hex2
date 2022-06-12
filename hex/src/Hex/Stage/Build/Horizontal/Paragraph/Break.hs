module Hex.Stage.Build.Horizontal.Paragraph.Break where

import Formatting qualified as F
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.ListElem qualified as H.Inter.B.List
import Hex.Stage.Build.Horizontal.Badness
import Hex.Stage.Build.Horizontal.Evaluate
import Hex.Stage.Build.Horizontal.Paragraph.Types
import Hexlude

data ChunkedHListElem
  = HListChunk (Seq H.Inter.B.List.HListElem)
  | HListBreakItem H.Inter.B.List.HListElem BreakItem
  deriving stock (Show, Generic)

fmtChunkedHListElem :: Fmt ChunkedHListElem
fmtChunkedHListElem = F.later $ \case
  HListChunk els -> bformat (F.commaSpaceSep H.Inter.B.List.fmtHListElem) els
  HListBreakItem _ b -> bformat ("Break: " |%| F.shown) b

chunkHList :: H.Inter.B.List.HList -> Seq ChunkedHListElem
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

finaliseHList :: H.Inter.B.List.HList -> H.Inter.B.List.HList
finaliseHList (H.Inter.B.List.HList Empty) =
  H.Inter.B.List.HList mempty
finaliseHList (H.Inter.B.List.HList elems@(elemInit :|> lastElem)) =
  let -- Remove the final item if it's glue.
      trimmedElems = case lastElem of
        H.Inter.B.List.HVListElem (H.Inter.B.List.ListGlue _) -> elemInit
        _ -> elems
      -- Add extra bits to finish the list.
      finishedElems = trimmedElems >< finishingElems
   in H.Inter.B.List.HList finishedElems
  where
    -- \penalty10k \hfil \penalty-10k.
    finishingElems :: Seq H.Inter.B.List.HListElem
    finishingElems =
      Empty
        :|> H.Inter.B.List.HVListElem (H.Inter.B.List.ListPenalty $ H.Inter.B.List.Penalty $ Q.HexInt Q.tenK)
        :|> H.Inter.B.List.HVListElem (H.Inter.B.List.ListGlue (Q.filStretchGlue Q.bigFilLength))
        :|> H.Inter.B.List.HVListElem (H.Inter.B.List.ListPenalty $ H.Inter.B.List.Penalty $ Q.HexInt $ -Q.tenK)

newtype Line = Line {unLine :: Seq H.Inter.B.List.HListElem}
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
--   H.Inter.B.List.HList ->
--   m (Seq (H.Inter.B.Box.Box H.Inter.B.Box.HBox))
-- breakParagraph _ _ _ (H.Inter.B.List.HList Empty) =
--   pure mempty
-- breakParagraph dw tol lp hList = do
--   let elemsWithBreaks = finaliseHList hList
--   panic "not implemented"

data DiscardingState = Discarding | NotDiscarding

withBreaks :: H.Inter.B.List.HList -> Seq (H.Inter.B.List.HListElem, Maybe BreakItem)
withBreaks = seqOf (#unHList % to toAdjacents % folded % to (\adj@(_, e, _) -> (e, hListElemToBreakItem adj)))

breakGreedy ::
  Q.Length -> -- HSize
  Q.HexInt -> -- Tolerance
  Q.HexInt -> -- LinePenalty
  H.Inter.B.List.HList ->
  Seq H.Inter.B.List.HList
breakGreedy dw _tol _lp (H.Inter.B.List.HList allEs) =
  let finalisedHList = finaliseHList (H.Inter.B.List.HList allEs)

      eWBs :: Seq (H.Inter.B.List.HListElem, Maybe BreakItem)
      eWBs = withBreaks finalisedHList

      lineSeq = go (LineSequence Empty, Line Empty, NotDiscarding) eWBs
   in -- Map from the line breaking types to our result type.
      seqOf (#unLineSequence % folded % #unLine % to H.Inter.B.List.HList) lineSeq
  where
    go :: (LineSequence, Line, DiscardingState) -> Seq (H.Inter.B.List.HListElem, Maybe BreakItem) -> LineSequence
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
                let (_spec, b) = listFlexSpec (H.Inter.B.List.HList lnEs) dw
                 in if b < infBadness
                      then
                        let newLnSeq = lns :|> ln
                            newEs = case bI of
                              GlueBreak _ -> aEs
                              KernBreak _ -> aEs
                              PenaltyBreak _ -> rEs
                         in go (LineSequence newLnSeq, Line Empty, Discarding) newEs
                      else go (lnSeq, Line (lnEs :|> e), NotDiscarding) rEs
