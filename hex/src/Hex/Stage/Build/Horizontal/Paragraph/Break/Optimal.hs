module Hex.Stage.Build.Horizontal.Paragraph.Break.Optimal where

import Data.Sequence qualified as Seq
import Formatting qualified as F
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.AnyDirection.Breaking.Badness qualified as Bad
import Hex.Stage.Build.AnyDirection.Evaluate qualified as Eval
import Hex.Stage.Build.Horizontal.Evaluate
import Hex.Stage.Build.Horizontal.Paragraph.Break.Chunk
import Hex.Stage.Build.Horizontal.Paragraph.Break.Common
import Hex.Stage.Build.Horizontal.Paragraph.Demerit (Demerit)
import Hex.Stage.Build.Horizontal.Paragraph.Demerit qualified as Demerit
import Hex.Stage.Build.Horizontal.Paragraph.Types (HBreakItem)
import Hex.Stage.Build.Horizontal.Paragraph.Types qualified as H.Break
import Hex.Stage.Build.Horizontal.Paragraph.Types qualified as Para
import Hex.Stage.Build.ListElem (HList (HList), HListElem)
import Hex.Stage.Build.ListElem qualified as ListElem
import Hex.Stage.Build.Vertical.Page.Break qualified as V.Break
import Hexlude

-- Cache stuff.

-- In spirit this is a map from 'BreakpointId -> CompletedLineSequence', but we can store it in a sequential data structure.
newtype BreakpointToBestLineSequenceCache = BreakpointToBestLineSequenceCache {unCompletedLineSequenceCache :: Seq CompletedLineSequence}
  deriving stock (Show, Generic)

nextBreakpointId :: BreakpointToBestLineSequenceCache -> BreakpointId
nextBreakpointId (BreakpointToBestLineSequenceCache nodes) = BreakpointId $ Seq.length nodes

addBreakpointToCache :: BreakpointToBestLineSequenceCache -> CompletedLineSequence -> BreakpointToBestLineSequenceCache
addBreakpointToCache (BreakpointToBestLineSequenceCache nodes) x = BreakpointToBestLineSequenceCache $ nodes |> x

emptyBreakpointCache :: BreakpointToBestLineSequenceCache
emptyBreakpointCache = BreakpointToBestLineSequenceCache mempty

lookupLineSequenceUpToStartpoint :: BreakpointToBestLineSequenceCache -> ParaPointId -> CompletedLineSequence
lookupLineSequenceUpToStartpoint (BreakpointToBestLineSequenceCache breakpointCache) startPoint = case startPoint of
  StartOfParagraphPoint ->
    emptyLineSequence
  ParaBreakPoint breakpointId ->
    case Seq.lookup breakpointId.unBreakpointId breakpointCache of
      Nothing ->
        panic $ "Impossible! No node found at index " <> show breakpointId
      Just x ->
        x

data ParaPointId = StartOfParagraphPoint | ParaBreakPoint BreakpointId
  deriving stock (Show, Generic)

newtype BreakpointId = BreakpointId {unBreakpointId :: Int}
  deriving stock (Show, Generic)

-- Incomplete line.

data IncompleteLine = IncompleteLine
  { lineElements :: Seq HListElem,
    lineStartPoint :: ParaPointId,
    discarding :: DiscardingState
  }
  deriving stock (Show, Generic)

-- Set this line to 'discarding' so that discardable items won't be added.
newMidParaIncompleteLine :: BreakpointId -> HBreakItem -> IncompleteLine
newMidParaIncompleteLine n breakItem =
  let startLineElems = H.Break.hBreakItemAsListElemsPostBreak breakItem

      -- TODO: I made up this logic. Maybe we have to act as if the post-break
      -- elements appeared in the input as normal, e.g. we discard kerns in the
      -- post-break text until we see something else.
      discarding = case startLineElems of
        Empty -> Discarding
        _ -> NotDiscarding
   in IncompleteLine startLineElems (ParaBreakPoint n) discarding

startOfParaIncompleteLine :: IncompleteLine
startOfParaIncompleteLine = IncompleteLine mempty StartOfParagraphPoint NotDiscarding

incompleteLineAddElem :: IncompleteLine -> HListElem -> IncompleteLine
incompleteLineAddElem line x = case line.discarding of
  Discarding
    | Para.hListElemIsDiscardable x ->
        line
  _ ->
    line {lineElements = line.lineElements |> x, discarding = NotDiscarding}

incompleteLineAddElems :: Seq HListElem -> IncompleteLine -> IncompleteLine
incompleteLineAddElems xs e = foldl' incompleteLineAddElem e xs

addElemsToIncompleteLines :: Seq HListElem -> Seq IncompleteLine -> Seq IncompleteLine
addElemsToIncompleteLines elems = fmap (incompleteLineAddElems elems)

incompleteLineIsNotOverfull :: Q.Length -> IncompleteLine -> Bool
incompleteLineIsNotOverfull desiredWidth line =
  not (Eval.flexSpecIsOverfull (listFlexSpec (HList line.lineElements) desiredWidth))

removeOverfullIncompletedLines :: Q.Length -> Seq IncompleteLine -> Seq IncompleteLine
removeOverfullIncompletedLines desiredWidth = Seq.filter (incompleteLineIsNotOverfull desiredWidth)

addElemsAndPruneOverfull :: Q.Length -> Seq HListElem -> Seq IncompleteLine -> Seq IncompleteLine
addElemsAndPruneOverfull desiredWidth elems = removeOverfullIncompletedLines desiredWidth . addElemsToIncompleteLines elems

-- Completed line.

data CompletedLine = CompletedLine
  { lineElements :: Seq HListElem,
    lineStartPoint :: ParaPointId
  }
  deriving stock (Show, Generic)

completeIncompleteLine :: HBreakItem -> IncompleteLine -> CompletedLine
completeIncompleteLine br line =
  let completedLineElements = line.lineElements <> H.Break.hBreakItemAsListElemsPreBreak br
   in CompletedLine completedLineElements line.lineStartPoint

completeAndPruneUnacceptableLines ::
  Q.Length ->
  ListElem.Penalty ->
  ListElem.Penalty ->
  Q.HexInt ->
  HBreakItem ->
  Seq IncompleteLine ->
  Seq CompletedLine
completeAndPruneUnacceptableLines desiredWidth hyphenPenalty exHyphenPenalty tolerance breakItem =
  Seq.filter (completedLineIsAcceptable tolerance desiredWidth hyphenPenalty exHyphenPenalty breakItem)
    . fmap (completeIncompleteLine breakItem)

completedLineBadness :: Q.Length -> CompletedLine -> Bad.Badness
completedLineBadness desiredWidth line =
  let completedLineFlexSpec =
        (listFlexSpec (HList line.lineElements) desiredWidth)
   in Eval.glueFlexSpecBadness completedLineFlexSpec

completedLineIsAcceptable :: Q.HexInt -> Q.Length -> ListElem.Penalty -> ListElem.Penalty -> HBreakItem -> CompletedLine -> Bool
completedLineIsAcceptable tolerance desiredWidth hyphenPenalty exHyphenPenalty breakItem completedLine =
  H.Break.hBreakIsAcceptable hyphenPenalty exHyphenPenalty tolerance breakItem (completedLineBadness desiredWidth completedLine)

data CompletedLineSequence = CompletedLineSequence
  { unCompletedLineSequence :: Seq CompletedLine,
    sequenceDemerit :: Demerit
  }
  deriving stock (Show, Generic)

emptyLineSequence :: CompletedLineSequence
emptyLineSequence = CompletedLineSequence mempty Demerit.zeroDemerit

-- Algorithm.

data BreakingState = BreakingState
  { incompleteLines :: Seq IncompleteLine,
    breakToBestLineSequenceCache :: BreakpointToBestLineSequenceCache
  }
  deriving stock (Show, Generic)

initialBreakingState :: BreakingState
initialBreakingState =
  BreakingState
    { incompleteLines = Seq.singleton startOfParaIncompleteLine,
      breakToBestLineSequenceCache = emptyBreakpointCache
    }

appendBreakListElement ::
  (State BreakingState :> es, Log.HexLog :> es) =>
  Q.Length ->
  ListElem.Penalty ->
  ListElem.Penalty ->
  Q.HexInt ->
  Q.HexInt ->
  ChunkedHListItem ->
  Eff es ()
appendBreakListElement desiredWidth hyphenPenalty exHyphenPenalty tolerance linePenalty = \case
  item@(Chunk _) ->
    extendIncompleteLinesWithItem item
  item@(ChunkedBreakItem breakItem) -> do
    acceptableCompletedLines <-
      use @BreakingState
        (#incompleteLines % to (completeAndPruneUnacceptableLines desiredWidth hyphenPenalty exHyphenPenalty tolerance breakItem))
    case toNonEmptySeq acceptableCompletedLines of
      -- If we can't break at this point acceptably somehow, it's not really a break-point, so just treat
      -- the break item like a non-break item, like the above 'chunk' case.
      Nothing ->
        extendIncompleteLinesWithItem item
      Just someAcceptableCompletedLines -> do
        Log.debugLog $
          F.sformat
            ("Really considering break-item: " |%| H.Break.fmtHBreakItem)
            breakItem

        -- If we have some acceptable lines, then we could actually break
        -- here. So we have 2 tasks:
        -- 1. Update the incompleted lines:
        --    1a. Update the current incompleted lines, to include the break
        --    item element.
        --    1b. Add a new incompleted line, to include the possibility that
        --    we do in fact break here.
        -- 2. Work out the best way to break at this point. That doesn't
        --    depend on the later items, so we can work this out. This is an
        --    optimisation, so we don't work out the best way to break at
        --    point 'x' many times.

        -- Task 1.
        -- 1a.
        extendIncompleteLinesWithItem item
        -- 1b.
        hereBreakpointId <- getNextBreakpointId
        let newIncompleteLine = newMidParaIncompleteLine hereBreakpointId breakItem
        modifying @BreakingState #incompleteLines (|> newIncompleteLine)

        -- Task 2.
        breakToBestLineSequenceCache <- use @BreakingState #breakToBestLineSequenceCache
        let bestSequenceBreakingHere =
              bestLineSequenceBreakingHere
                desiredWidth
                linePenalty
                hyphenPenalty
                exHyphenPenalty
                breakItem
                breakToBestLineSequenceCache
                someAcceptableCompletedLines
        -- If we later need to form a line sequence involving a completed line that starts at this point,
        -- record the best way to reach this point.
        assign @BreakingState #breakToBestLineSequenceCache (addBreakpointToCache breakToBestLineSequenceCache bestSequenceBreakingHere)
  where
    -- TODO: Might not need to prune over-full in all cases.
    extendIncompleteLinesWithItem item =
      modifying @BreakingState
        #incompleteLines
        (addElemsAndPruneOverfull desiredWidth (chunkedListItemElems item))

    getNextBreakpointId = use @BreakingState (#breakToBestLineSequenceCache % to nextBreakpointId)

data NonEmptySeq a = NonEmptySeq a (Seq a)

instance Foldable NonEmptySeq where
  foldMap f (NonEmptySeq v vSeq) =
    f v <> foldMap f vSeq

instance Functor NonEmptySeq where
  fmap f (NonEmptySeq v vSeq) =
    NonEmptySeq (f v) (f <$> vSeq)

toNonEmptySeq :: Seq a -> Maybe (NonEmptySeq a)
toNonEmptySeq = \case
  Empty -> Nothing
  x :<| xs -> Just (NonEmptySeq x xs)

bestLineSequenceBreakingHere ::
  Q.Length ->
  Q.HexInt ->
  ListElem.Penalty ->
  ListElem.Penalty ->
  HBreakItem ->
  BreakpointToBestLineSequenceCache ->
  -- | Assumed to be non-empty.
  NonEmptySeq CompletedLine ->
  CompletedLineSequence
bestLineSequenceBreakingHere desiredWidth linePenalty hyphenPenalty exHyphenPenalty breakItem breakToBestLineSequenceCache acceptableCompletedLines =
  -- TODO: Recomputing badness here might not be great. Probably better to cache it when we are finding 'acceptable' lines.
  let acceptableLinesWithDemerits = acceptableCompletedLines <&> (\ln -> (ln, Demerit.demerit linePenalty hyphenPenalty exHyphenPenalty (completedLineBadness desiredWidth ln) breakItem))
   in minimumBy
        (\lineSeqA lineSeqB -> lineSeqA.sequenceDemerit `compare` lineSeqB.sequenceDemerit)
        (addLineToLineSequence breakToBestLineSequenceCache <$> acceptableLinesWithDemerits)

addLineToLineSequence :: BreakpointToBestLineSequenceCache -> (CompletedLine, Demerit) -> CompletedLineSequence
addLineToLineSequence breakpointCache (ln@(CompletedLine _elems src), lnDemerit) =
  let (CompletedLineSequence rSoln demerit) = lookupLineSequenceUpToStartpoint breakpointCache src
   in CompletedLineSequence (rSoln |> ln) (Demerit.combineDemerits demerit lnDemerit)

appendAllElements ::
  Log.HexLog :> es =>
  Q.Length -> -- HSize
  ListElem.Penalty -> -- hyphenpenalty
  ListElem.Penalty -> -- exhyphenpenalty
  Q.HexInt -> -- Tolerance
  Q.HexInt -> -- LinePenalty
  ListElem.HList ->
  Eff es BreakingState
appendAllElements desiredWidth hyphenPenalty exHyphenPenalty tolerance lp (ListElem.HList allEs) =
  let finalisedHList = prepareHListForBreaking (ListElem.HList allEs)
      breakList = asChunkedList finalisedHList
   in execStateLocal initialBreakingState (for_ breakList (appendBreakListElement desiredWidth hyphenPenalty exHyphenPenalty tolerance lp))

finaliseBrokenList ::
  Q.Length ->
  ListElem.Penalty -> -- hyphenpenalty
  ListElem.Penalty -> -- exhyphenpenalty
  Q.HexInt ->
  Q.HexInt ->
  BreakingState ->
  Maybe (Seq HList)
finaliseBrokenList desiredWidth hyphenPenalty exHyphenPenalty tolerance linePenalty finalState =
  -- Finish off by 'breaking' at the end of the list.
  -- TODO: Do this properly.
  let finalFakePenalty = ListElem.Penalty $ Q.zeroInt
      finalBreakItem = H.Break.HVBreakItem (V.Break.PenaltyBreak finalFakePenalty)
      acceptableCompletedLines =
        finalState
          ^. ( (#incompleteLines % to (completeAndPruneUnacceptableLines desiredWidth hyphenPenalty exHyphenPenalty tolerance finalBreakItem))
             )
   in case toNonEmptySeq acceptableCompletedLines of
        Nothing ->
          Nothing
        Just someAcceptableCompletedLines ->
          let bestSeq =
                bestLineSequenceBreakingHere
                  desiredWidth
                  linePenalty
                  hyphenPenalty
                  exHyphenPenalty
                  finalBreakItem
                  finalState.breakToBestLineSequenceCache
                  someAcceptableCompletedLines
           in Just $
                bestSeq.unCompletedLineSequence <&> \completedLine ->
                  ListElem.HList $ completedLine.lineElements

breakHListOptimally ::
  Log.HexLog :> es =>
  Q.Length -> -- HSize
  ListElem.Penalty -> -- hyphenpenalty
  ListElem.Penalty -> -- exhyphenpenalty
  Q.HexInt -> -- Tolerance
  Q.HexInt -> -- LinePenalty
  ListElem.HList ->
  Eff es (Maybe (Seq HList))
breakHListOptimally desiredWidth hyphenPenalty exHyphenPenalty tolerance linePenalty hList = do
  finalState <- appendAllElements desiredWidth hyphenPenalty exHyphenPenalty tolerance linePenalty hList
  pure $ finaliseBrokenList desiredWidth hyphenPenalty exHyphenPenalty tolerance linePenalty finalState
