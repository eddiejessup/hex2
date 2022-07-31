module Hex.Stage.Build.Horizontal.Paragraph.Break.Greedy where

import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.AnyDirection.Breaking.Badness qualified as Bad
import Hex.Stage.Build.AnyDirection.Breaking.Types (BreakItem)
import Hex.Stage.Build.AnyDirection.Breaking.Types qualified as Break
import Hex.Stage.Build.AnyDirection.Evaluate qualified as Eval
import Hex.Stage.Build.Horizontal.Evaluate
import Hex.Stage.Build.Horizontal.Paragraph.Break.Chunk
import Hex.Stage.Build.Horizontal.Paragraph.Break.Common
import Hex.Stage.Build.Horizontal.Paragraph.Types
import Hex.Stage.Build.ListElem (HListElem)
import Hex.Stage.Build.ListElem qualified as ListElem
import Hexlude

newtype Line = Line {unLine :: Seq HListElem}
  deriving stock (Show, Generic)

newtype LineSequence = LineSequence {unLineSequence :: Seq Line}
  deriving stock (Show, Generic)

breakGreedy ::
  Q.Length -> -- HSize
  Q.HexInt -> -- Tolerance
  Q.HexInt -> -- LinePenalty
  ListElem.HList ->
  Seq ListElem.HList
breakGreedy desiredWidth _tol _lp (ListElem.HList allEs) =
  let finalisedHList = finaliseHList (ListElem.HList allEs)

      eWBs :: Seq (HListElem, Maybe BreakItem)
      eWBs = withBreaks finalisedHList

      lineSeq = go (LineSequence Empty, Line Empty, NotDiscarding) eWBs
   in -- Map from the line breaking types to our result type.
      seqOf (#unLineSequence % folded % #unLine % to ListElem.HList) lineSeq
  where
    go :: (LineSequence, Line, DiscardingState) -> Seq (HListElem, Maybe BreakItem) -> LineSequence
    go st@(lnSeq@(LineSequence lns), ln@(Line lnEs), discarding) = \case
      Empty -> LineSequence (lns :|> ln)
      aEs@((e, mayBI) :<| rEs) ->
        case discarding of
          Discarding
            | hListElemIsDiscardable e ->
                go st rEs
          _ ->
            case mayBI of
              Nothing ->
                go (lnSeq, Line (lnEs :|> e), NotDiscarding) rEs
              Just bI ->
                let spec = listFlexSpec (ListElem.HList lnEs) desiredWidth
                    b = Eval.glueFlexSpecBadness spec
                 in case b of
                      Bad.InfiniteBadness ->
                        let newLnSeq = lns :|> ln
                            newEs = case bI of
                              Break.GlueBreak _ -> aEs
                              Break.KernBreak _ -> aEs
                              Break.PenaltyBreak _ -> rEs
                         in go (LineSequence newLnSeq, Line Empty, Discarding) newEs
                      Bad.FiniteBadness _finiteBadness ->
                        go (lnSeq, Line (lnEs :|> e), NotDiscarding) rEs
