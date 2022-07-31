module Hex.Stage.Build.Horizontal.Paragraph.Break.Optimal where

import Data.Sequence qualified as Seq
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.AnyDirection.Breaking.Badness qualified as Bad
import Hex.Stage.Build.AnyDirection.Breaking.Types (BreakItem (..))
import Hex.Stage.Build.AnyDirection.Evaluate qualified as Eval
import Hex.Stage.Build.Horizontal.Evaluate
import Hex.Stage.Build.Horizontal.Paragraph.Break.Chunk
import Hex.Stage.Build.Horizontal.Paragraph.Break.Common
import Hex.Stage.Build.Horizontal.Paragraph.Demerit (Demerit)
import Hex.Stage.Build.Horizontal.Paragraph.Demerit qualified as Demerit
import Hex.Stage.Build.Horizontal.Paragraph.Types qualified as Para
import Hex.Stage.Build.ListElem (HList (HList), HListElem)
import Hex.Stage.Build.ListElem qualified as ListElem
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
newMidParaIncompleteLine :: BreakpointId -> IncompleteLine
newMidParaIncompleteLine n = IncompleteLine mempty (ParaBreakPoint n) Discarding

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

-- Add the break-point unless it's glue.
completeIncompleteLine :: HListElem -> IncompleteLine -> CompletedLine
completeIncompleteLine x line = case x of
  ListElem.HVListElem (ListElem.ListGlue _) ->
    CompletedLine (line.lineElements) (line.lineStartPoint)
  _ ->
    CompletedLine (line.lineElements |> x) (line.lineStartPoint)

completedLineBadness :: Q.Length -> CompletedLine -> Bad.Badness
completedLineBadness desiredWidth line =
  let completedLineFlexSpec =
        (listFlexSpec (HList line.lineElements) desiredWidth)
   in Eval.glueFlexSpecBadness completedLineFlexSpec

completedLineIsAcceptable :: Q.HexInt -> Q.Length -> BreakItem -> CompletedLine -> Bool
completedLineIsAcceptable tolerance desiredWidth breakItem completedLine =
  Bad.breakIsAcceptable tolerance breakItem (completedLineBadness desiredWidth completedLine)

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
  State BreakingState :> es =>
  Q.Length ->
  Q.HexInt ->
  Q.HexInt ->
  ChunkedListItem ->
  Eff es ()
appendBreakListElement desiredWidth tolerance linePenalty item = do
  -- TODO: Might not need to prune over-full in all cases.
  incompleteLinesPlusItemElems <- use @BreakingState (#incompleteLines % to (addElemsAndPruneOverfull desiredWidth (chunkedListItemElems item)))
  case item of
    Chunk _ ->
      assign @BreakingState (#incompleteLines) incompleteLinesPlusItemElems
    ChunkedBreakItem breakItemElem breakItem -> do
      acceptableCompletedLines <-
        use @BreakingState
          ( #incompleteLines
              % to (fmap ((completeIncompleteLine breakItemElem)))
              % to (Seq.filter (completedLineIsAcceptable tolerance desiredWidth breakItem))
          )
      case acceptableCompletedLines of
        -- If we can't break at this point acceptably somehow, it's not really a break-point, so just treat
        -- the break item like a non-break item, like the above 'chunk' case.
        Empty ->
          assign @BreakingState (#incompleteLines) incompleteLinesPlusItemElems
        _ -> do
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
          hereBreakpointId <- getNextBreakpointId
          assign @BreakingState #incompleteLines (incompleteLinesPlusItemElems |> (newMidParaIncompleteLine hereBreakpointId))

          -- Task 2.
          breakToBestLineSequenceCache <- use @BreakingState #breakToBestLineSequenceCache
          let bestSequenceBreakingHere = bestLineSequenceBreakingHere desiredWidth linePenalty breakItem breakToBestLineSequenceCache acceptableCompletedLines
          -- If we later need to form a line sequence involving a completed line that starts at this point,
          -- record the best way to reach this point.
          assign @BreakingState #breakToBestLineSequenceCache (addBreakpointToCache breakToBestLineSequenceCache bestSequenceBreakingHere)
  where
    getNextBreakpointId = use @BreakingState (#breakToBestLineSequenceCache % to nextBreakpointId)

bestLineSequenceBreakingHere ::
  Q.Length ->
  Q.HexInt ->
  BreakItem ->
  BreakpointToBestLineSequenceCache ->
  Seq CompletedLine ->
  CompletedLineSequence
bestLineSequenceBreakingHere desiredWidth linePenalty breakItem breakToBestLineSequenceCache acceptableCompletedLines =
  -- TODO: Recomputing badness here might not be great. Probably better to cache it when we are finding 'acceptable' lines.
  let acceptableLinesWithDemerits = acceptableCompletedLines <&> (\ln -> (ln, Demerit.demerit linePenalty (completedLineBadness desiredWidth ln) breakItem))
   in minimumBy
        (\lineSeqA lineSeqB -> lineSeqA.sequenceDemerit `compare` lineSeqB.sequenceDemerit)
        (addLineToLineSequence breakToBestLineSequenceCache <$> acceptableLinesWithDemerits)

addLineToLineSequence :: BreakpointToBestLineSequenceCache -> (CompletedLine, Demerit) -> CompletedLineSequence
addLineToLineSequence breakpointCache (ln@(CompletedLine _elems src), lnDemerit) =
  let (CompletedLineSequence rSoln demerit) = lookupLineSequenceUpToStartpoint breakpointCache src
   in CompletedLineSequence (rSoln |> ln) (Demerit.combineDemerits demerit lnDemerit)

appendAllElements ::
  Q.Length -> -- HSize
  Q.HexInt -> -- Tolerance
  Q.HexInt -> -- LinePenalty
  ListElem.HList ->
  Eff es BreakingState
appendAllElements desiredWidth tolerance lp (ListElem.HList allEs) =
  let finalisedHList = finaliseHList (ListElem.HList allEs)
      breakList = asChunkedList finalisedHList
   in execStateLocal initialBreakingState (for_ breakList (appendBreakListElement desiredWidth tolerance lp))

foo ::
  Q.Length -> -- HSize
  Q.HexInt -> -- Tolerance
  Q.HexInt -> -- LinePenalty
  ListElem.HList ->
  Eff es (Seq HList)
foo desiredWidth tolerance linePenalty hList = do
  finalState <- appendAllElements desiredWidth tolerance linePenalty hList
  -- Finish off by 'breaking' at the end of the list.
  -- TODO: Do this properly.
  let finalFakePenalty = ListElem.Penalty $ Q.zeroInt
      finalFakeElem = ListElem.HVListElem $ ListElem.ListPenalty finalFakePenalty
      finalBreakItem = PenaltyBreak finalFakePenalty
      acceptableCompletedLines =
        finalState
          ^. ( #incompleteLines
                 % to (fmap ((completeIncompleteLine finalFakeElem)))
                 % to (Seq.filter (completedLineIsAcceptable tolerance desiredWidth finalBreakItem))
             )
      bestSequence =
        bestLineSequenceBreakingHere
          desiredWidth
          linePenalty
          finalBreakItem
          finalState.breakToBestLineSequenceCache
          acceptableCompletedLines
  pure $
    bestSequence.unCompletedLineSequence <&> \completedLine -> ListElem.HList $ completedLine.lineElements
