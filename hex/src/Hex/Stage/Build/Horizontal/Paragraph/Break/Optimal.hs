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
import Hex.Stage.Build.ListElem (HListElem)
import Hex.Stage.Build.ListElem qualified as ListElem
import Hex.Stage.Build.Vertical.Page.Break qualified as V.Break
import Hexlude
import Hexlude.NonEmptySeq qualified as Seq.NE
import Witherable qualified as Wither

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

lineFlexSpec :: HasField "lineElements" r (Seq HListElem) => Q.Length -> r -> Eval.GlueFlexSpec
lineFlexSpec desiredWidth line = listFlexSpec line.lineElements desiredWidth

incompleteLineIsNotOverfull :: Q.Length -> IncompleteLine -> Bool
incompleteLineIsNotOverfull desiredWidth line =
  not (Eval.flexSpecIsOverfull (lineFlexSpec desiredWidth line))

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

completeIncompleteLine :: HBreakItem -> Q.Glue -> Q.Glue -> IncompleteLine -> CompletedLine
completeIncompleteLine br leftSkip rightSkip line =
  let completedLineElements =
        ( (ListElem.HVListElem (ListElem.ListGlue leftSkip))
            <| line.lineElements
              <> H.Break.hBreakItemAsListElemsPreBreak br
        )
          |> (ListElem.HVListElem (ListElem.ListGlue rightSkip))
   in CompletedLine completedLineElements line.lineStartPoint

completeAndPruneUnacceptableLines ::
  (Reader LineBreakingEnv :> es) =>
  HBreakItem ->
  Seq IncompleteLine ->
  Eff es (Seq CompletedLine)
completeAndPruneUnacceptableLines breakItem incompleteds = do
  leftSkip <- know @LineBreakingEnv #leftSkip
  rightSkip <- know @LineBreakingEnv #rightSkip
  let completeds = completeIncompleteLine breakItem leftSkip rightSkip <$> incompleteds
  Wither.filterA (completedLineIsAcceptable breakItem) completeds

completedLineBadness :: Reader LineBreakingEnv :> es => CompletedLine -> Eff es Bad.Badness
completedLineBadness line = do
  desiredWidth <- know @LineBreakingEnv #hSize
  let completedLineFlexSpec = lineFlexSpec desiredWidth line
  pure $ Eval.glueFlexSpecBadness completedLineFlexSpec

completedLineIsAcceptable :: Reader LineBreakingEnv :> es => HBreakItem -> CompletedLine -> Eff es Bool
completedLineIsAcceptable breakItem completedLine =
  completedLineBadness completedLine >>= H.Break.hBreakIsAcceptable breakItem

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
  (Reader LineBreakingEnv :> es, State BreakingState :> es, Log.HexLog :> es) =>
  ChunkedHListItem ->
  Eff es ()
appendBreakListElement = \case
  item@(Chunk _) ->
    extendIncompleteLinesWithItem item
  item@(ChunkedBreakItem breakItem) -> do
    acceptableCompletedLines <-
      use @BreakingState #incompleteLines
        >>= completeAndPruneUnacceptableLines breakItem

    case Seq.NE.nonEmpty acceptableCompletedLines of
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
        bestSequenceBreakingHere <-
          bestLineSequenceBreakingHere
            breakItem
            breakToBestLineSequenceCache
            someAcceptableCompletedLines
        -- If we later need to form a line sequence involving a completed line that starts at this point,
        -- record the best way to reach this point.
        assign @BreakingState #breakToBestLineSequenceCache (addBreakpointToCache breakToBestLineSequenceCache bestSequenceBreakingHere)
  where
    -- TODO: Might not need to prune over-full in all cases.
    extendIncompleteLinesWithItem item = do
      desiredWidth <- know @LineBreakingEnv #hSize
      modifying @BreakingState
        #incompleteLines
        (addElemsAndPruneOverfull desiredWidth (chunkedListItemElems item))

    getNextBreakpointId = use @BreakingState (#breakToBestLineSequenceCache % to nextBreakpointId)

bestLineSequenceBreakingHere ::
  Reader LineBreakingEnv :> es =>
  HBreakItem ->
  BreakpointToBestLineSequenceCache ->
  NonEmptySeq CompletedLine ->
  Eff es CompletedLineSequence
bestLineSequenceBreakingHere breakItem breakToBestLineSequenceCache acceptableCompletedLines = do
  -- TODO: Recomputing badness here might not be great. Probably better to cache it when we are finding 'acceptable' lines.
  acceptableLinesWithDemerits <- for acceptableCompletedLines $ \ln -> do
    linePenalty <- know @LineBreakingEnv #linePenalty
    hyphenPenalty <- know @LineBreakingEnv #hyphenPenalty
    exHyphenPenalty <- know @LineBreakingEnv #exHyphenPenalty
    bad <- completedLineBadness ln
    let demerit = Demerit.demerit linePenalty hyphenPenalty exHyphenPenalty bad breakItem
    pure (ln, demerit)
  pure $
    minimumBy
      (\lineSeqA lineSeqB -> lineSeqA.sequenceDemerit `compare` lineSeqB.sequenceDemerit)
      (addLineToLineSequence breakToBestLineSequenceCache <$> acceptableLinesWithDemerits)

addLineToLineSequence :: BreakpointToBestLineSequenceCache -> (CompletedLine, Demerit) -> CompletedLineSequence
addLineToLineSequence breakpointCache (ln@(CompletedLine _elems src), lnDemerit) =
  let (CompletedLineSequence rSoln demerit) = lookupLineSequenceUpToStartpoint breakpointCache src
   in CompletedLineSequence (rSoln |> ln) (Demerit.combineDemerits demerit lnDemerit)

appendAllElements ::
  (Reader LineBreakingEnv :> es, Log.HexLog :> es) =>
  Seq ListElem.HListElem ->
  Eff es BreakingState
appendAllElements allEs =
  let finalisedHList = prepareHListForBreaking allEs
      breakList = asChunkedList finalisedHList
   in execStateLocal
        initialBreakingState
        (for_ breakList appendBreakListElement)

finaliseBrokenList ::
  (Reader LineBreakingEnv :> es) =>
  BreakingState ->
  Eff es (Maybe (Seq (Seq HListElem)))
finaliseBrokenList finalState = do
  -- Finish off by 'breaking' at the end of the list.
  -- TODO: Do this properly.
  let finalBreakItem = H.Break.HVBreakItem (V.Break.PenaltyBreak Bad.zeroFiniteBadness)
  acceptableCompletedLines <- completeAndPruneUnacceptableLines finalBreakItem finalState.incompleteLines
  case Seq.NE.nonEmpty acceptableCompletedLines of
    Nothing ->
      pure Nothing
    Just someAcceptableCompletedLines -> do
      bestSeq <-
        bestLineSequenceBreakingHere
          finalBreakItem
          finalState.breakToBestLineSequenceCache
          someAcceptableCompletedLines
      pure $
        Just $
          bestSeq.unCompletedLineSequence <&> \completedLine ->
            completedLine.lineElements

breakHListOptimally ::
  (Reader LineBreakingEnv :> es, Log.HexLog :> es) =>
  Seq ListElem.HListElem ->
  Eff es (Maybe (Seq (Seq HListElem)))
breakHListOptimally hList = do
  finalState <- appendAllElements hList
  finaliseBrokenList finalState
