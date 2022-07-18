module Hex.Stage.Build.Vertical.Page.Break where

import Data.Sequence qualified as Seq
import Formatting qualified as F
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.AnyDirection.Breaking.Badness qualified as Bad
import Hex.Stage.Build.AnyDirection.Breaking.Types qualified as Breaking
import Hex.Stage.Build.AnyDirection.Evaluate qualified as Eval
import Hex.Stage.Build.ListElem (VList (..))
import Hex.Stage.Build.ListElem qualified as List
import Hex.Stage.Build.ListExtractor.HList ()
import Hex.Stage.Build.Vertical.Evaluate qualified as Eval
import Hex.Stage.Build.Vertical.Page.Types qualified as Page
import Hex.Stage.Build.Vertical.Set qualified as Set
import Hexlude

data CurrentPage = CurrentPage
  { items :: VList,
    bestPointAndCost :: BestPointAndCost
  }
  deriving stock (Show, Generic)

data BestPointAndCost = BestPointAndCost
  { ix :: Int,
    cost :: PageCost
  }
  deriving stock (Show, Generic)

data PageCost
  = InfiniteCost
  | FiniteCost FiniteCostValue
  deriving stock (Show, Eq, Generic)

instance Ord PageCost where
  InfiniteCost `compare` InfiniteCost = EQ
  InfiniteCost `compare` FiniteCost _ = GT
  FiniteCost _ `compare` InfiniteCost = LT
  FiniteCost fA `compare` FiniteCost fB = fA `compare` fB

newtype FiniteCostValue = FiniteCostValue {unFiniteCostValue :: Int}
  deriving stock (Show, Eq, Ord, Generic)

-- | Initialise a page with the largest finite cost, because an infinite cost signals 'stop and break here', which we don't want.
newCurrentPage :: CurrentPage
newCurrentPage = CurrentPage mempty (BestPointAndCost 0 (FiniteCost (FiniteCostValue Q.hunK)))

currentPageBestCost :: CurrentPage -> PageCost
currentPageBestCost currentPage = currentPage.bestPointAndCost.cost

updateWithCost :: PageCost -> CurrentPage -> CurrentPage
updateWithCost newCost currentPage
  | newCost <= currentPageBestCost currentPage =
      currentPage & #bestPointAndCost !~ BestPointAndCost (currentPageIndex currentPage) newCost
  | otherwise = currentPage

currentPageIndex :: CurrentPage -> Int
currentPageIndex currentPage = Seq.length currentPage.items.unVList

currentPageLastElem :: CurrentPage -> Maybe List.VListElem
currentPageLastElem currentPage = seqLastMay (currentPage.items.unVList)

breakPageAtBest :: CurrentPage -> (VList, VList)
breakPageAtBest currentPage =
  let (pagePart, leftovers) = Seq.splitAt (currentPage.bestPointAndCost.ix) (currentPage.items.unVList)
   in (VList pagePart, VList leftovers)

setPage :: Q.Length -> VList -> Page.Page
setPage desiredHeight vList =
  let (vBoxElems, _) = Set.setList vList desiredHeight
   in Page.Page vBoxElems

data PageBreakJudgment
  = DoNotBreak
  | BreakPageAtBest
  | BreakPageHere
  | TrackCost !Int
  deriving stock (Show, Generic)

pageBreakCost ::
  Log.MonadHexLog m =>
  CurrentPage ->
  Breaking.BreakItem ->
  Q.Length ->
  m PageCost
pageBreakCost currentPage breakItem desiredHeight = do
  let flexSpec = Eval.listFlexSpec currentPage.items desiredHeight
      b = Eval.glueFlexSpecBadness flexSpec
      p = Breaking.breakPenalty breakItem
      -- TODO: q is ‘\insertpenalties’, the sum of all penalties for split
      -- insertions on the page, as explained below.
      q = 0
  Log.infoLog $ "Flex spec: " <> show flexSpec <> ", desiredHeight" <> show desiredHeight
  Log.infoLog $ "Badness: " <> show b <> ", penalty: " <> show p
  pure $ case b of
    Bad.InfiniteBadness ->
      InfiniteCost
    Bad.FiniteBadness finiteB
      | q >= Q.tenK -> InfiniteCost
      | p <= -Q.tenK -> FiniteCost $ FiniteCostValue $ p
      | finiteB == Bad.maxFiniteBadness -> FiniteCost $ FiniteCostValue Q.hunK
      | otherwise -> FiniteCost $ FiniteCostValue (finiteB.unFiniteBadnessVal + p + q)

-- If c = ∞ or if p ≤ −10000, Tex seizes the initiative and breaks the page at the best remembered breakpoint.
costImpliesBreakHere :: PageCost -> Bool
costImpliesBreakHere = \case
  InfiniteCost -> True
  FiniteCost finiteCost -> finiteCost <= FiniteCostValue (-Q.tenK)

runPageBuilder ::
  forall m.
  Log.MonadHexLog m =>
  Q.Length ->
  VList ->
  m (Seq Page.Page)
runPageBuilder desiredHeight vList =
  go newCurrentPage vList
  where
    go :: CurrentPage -> VList -> m (Seq Page.Page)
    go currentPage@(CurrentPage curPageElems _bestPointAndCost) toAddElems@(VList toAddElemSeq) =
      case toAddElemSeq of
        Empty ->
          pure $ Seq.singleton (setPage desiredHeight curPageElems)
        x :<| xs ->
          let continueToNextElem newPage = go newPage (VList xs)

              currentPageWithAddedElem = currentPage & #items % #unVList %~ (|> x)
           in if not (List.vListContainsBoxes curPageElems)
                then -- If the current vlist has no boxes, we discard a discardable item.
                -- Otherwise, if a discardable item is a legitimate breakpoint, we compute
                -- the cost c of breaking at this point.

                  if Breaking.vListElemIsDiscardable x
                    then do
                      Log.infoLog $ "Continuing, cos no boxes, with discardable elem: " <> F.sformat List.fmtVListElem x
                      continueToNextElem currentPage
                    else do
                      Log.infoLog $ "Continuing, cos no boxes, with non-discardable elem: " <> F.sformat List.fmtVListElem x
                      continueToNextElem currentPageWithAddedElem
                else case Breaking.vListElemToBreakItem (currentPageLastElem currentPage, x, seqHeadMay xs) of
                  -- If we can't break here, just add it to the list and continue.
                  Nothing -> do
                    Log.infoLog ("Continuing, non-break item: " <> F.sformat List.fmtVListElem x)
                    continueToNextElem currentPageWithAddedElem
                  Just breakItem -> do
                    cost <- pageBreakCost currentPageWithAddedElem breakItem desiredHeight
                    if costImpliesBreakHere cost
                      then do
                        Log.infoLog $ "Breaking at cost: " <> show cost <> " with current-page length: " <> show (Seq.length (currentPage.items.unVList))
                        let (newPageElems, leftoverListElems) = breakPageAtBest currentPage
                        Log.infoLog $
                          "Broken page length: " <> show (Seq.length newPageElems.unVList)
                            <> ", leftover elems length: "
                            <> show (Seq.length leftoverListElems.unVList)
                            <> ", remaining length: "
                            <> show (Seq.length toAddElems.unVList)
                        rest <- go newCurrentPage (leftoverListElems <> toAddElems)
                        pure $ setPage desiredHeight newPageElems <| rest
                      else do
                        -- If the resulting cost <= the smallest cost seen so far, remember
                        -- the current breakpoint as the best so far.
                        Log.infoLog $ "Got non-breakey cost: " <> show cost <> "compare to best: " <> show (currentPageBestCost currentPageWithAddedElem)
                        continueToNextElem (updateWithCost cost currentPageWithAddedElem)