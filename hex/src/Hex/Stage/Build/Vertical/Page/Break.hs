module Hex.Stage.Build.Vertical.Page.Break where

import Data.Sequence qualified as Seq
import Formatting qualified as F
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.AnyDirection.Breaking.Badness (FiniteBadnessVal (FiniteBadnessVal))
import Hex.Stage.Build.AnyDirection.Breaking.Badness qualified as Bad
import Hex.Stage.Build.AnyDirection.Evaluate qualified as Eval
import Hex.Stage.Build.BoxElem qualified as BoxElem
import Hex.Stage.Build.ListElem (VList (..))
import Hex.Stage.Build.ListElem qualified as ListElem
import Hex.Stage.Build.Vertical.Evaluate qualified as Eval
import Hex.Stage.Build.Vertical.Page.Types qualified as Page
import Hex.Stage.Build.Vertical.Set qualified as Set
import Hexlude

data VBreakItem
  = GlueBreak Q.Glue
  | KernBreak BoxElem.Kern
  | PenaltyBreak Bad.FiniteBadnessVal
  deriving stock (Show, Generic)

vListElemToBreakItem :: (Maybe ListElem.VListElem, ListElem.VListElem, Maybe ListElem.VListElem) -> Maybe VBreakItem
vListElemToBreakItem = \case
  (Just x, ListElem.ListGlue g, _)
    | not (vListElemIsDiscardable x) ->
        Just $ GlueBreak g
  (_, ListElem.VListBaseElem (BoxElem.ElemKern k), Just (ListElem.ListGlue _)) ->
    Just $ KernBreak k
  (_, ListElem.ListPenalty p, _) ->
    Just $ PenaltyBreak p
  _ ->
    Nothing

vListElemIsDiscardable :: ListElem.VListElem -> Bool
vListElemIsDiscardable = \case
  ListElem.ListGlue _ -> True
  ListElem.ListPenalty _ -> True
  ListElem.VListBaseElem (BoxElem.ElemKern _) -> True
  ListElem.VListBaseElem (BoxElem.ElemBox _) -> False

vBreakPenalty :: VBreakItem -> Bad.FiniteBadnessVal
vBreakPenalty (PenaltyBreak p) = p
vBreakPenalty (GlueBreak _) = Bad.zeroFiniteBadness
vBreakPenalty (KernBreak _) = Bad.zeroFiniteBadness

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

newtype FiniteCostValue = FiniteCostValue {unFiniteCostValue :: FiniteBadnessVal}
  deriving stock (Show, Eq, Ord, Generic)

-- | Initialise a page with the largest finite cost, because an infinite cost signals 'stop and break here', which we don't want.
newCurrentPage :: CurrentPage
newCurrentPage = CurrentPage mempty (BestPointAndCost 0 (FiniteCost (FiniteCostValue $ FiniteBadnessVal Q.hunKInt)))

currentPageBestCost :: CurrentPage -> PageCost
currentPageBestCost currentPage = currentPage.bestPointAndCost.cost

updateWithCost :: PageCost -> CurrentPage -> CurrentPage
updateWithCost newCost currentPage
  | newCost <= currentPageBestCost currentPage =
      currentPage & #bestPointAndCost !~ BestPointAndCost (currentPageIndex currentPage) newCost
  | otherwise = currentPage

currentPageIndex :: CurrentPage -> Int
currentPageIndex currentPage = Seq.length currentPage.items.unVList

currentPageLastElem :: CurrentPage -> Maybe ListElem.VListElem
currentPageLastElem currentPage = seqLastMay (currentPage.items.unVList)

breakPageAtBest :: CurrentPage -> (VList, VList)
breakPageAtBest currentPage =
  let (pagePart, leftovers) = Seq.splitAt (currentPage.bestPointAndCost.ix) (currentPage.items.unVList)
   in (VList pagePart, VList leftovers)

setPage :: Log.HexLog :> es => Q.Length -> VList -> Eff es Page.Page
setPage desiredHeight vList = do
  let listHeight = ListElem.vListNaturalHeight vList
      (vBoxElems, flexSpec) = Set.setList vList desiredHeight
  Log.infoLog $
    F.sformat
      ( "setPage: setting at page height: "
          |%| Q.fmtLengthWithUnit
          |%| ", list natural-height: "
          |%| Q.fmtLengthWithUnit
          |%| ", got flex-spec: "
          |%| Eval.fmtGlueFlexSpec
      )
      desiredHeight
      listHeight
      flexSpec
  pure $ Page.Page vBoxElems

data PageBreakJudgment
  = DoNotBreak
  | BreakPageAtBest
  | BreakPageHere
  | TrackCost !Int
  deriving stock (Show, Generic)

pageBreakCost ::
  Log.HexLog :> es =>
  CurrentPage ->
  VBreakItem ->
  Q.Length ->
  Eff es PageCost
pageBreakCost currentPage breakItem desiredHeight = do
  let flexSpec = Eval.listFlexSpec currentPage.items desiredHeight
      b = Eval.glueFlexSpecBadness flexSpec
      p = vBreakPenalty breakItem
      -- TODO: q is ‘\insertpenalties’, the sum of all penalties for split
      -- insertions on the page, as explained below.
      q = Bad.zeroFiniteBadness
  Log.infoLog $ "Flex spec: " <> show flexSpec <> ", desiredHeight" <> show desiredHeight
  Log.infoLog $ "Badness: " <> show b <> ", penalty: " <> show p
  pure $ case b of
    Bad.InfiniteBadness ->
      InfiniteCost
    Bad.FiniteBadness finiteB
      | q >= FiniteBadnessVal Q.tenKInt -> InfiniteCost
      | p <= (FiniteBadnessVal (invert Q.tenKInt)) -> FiniteCost $ FiniteCostValue p
      | finiteB == Bad.maxFiniteBadness -> FiniteCost $ FiniteCostValue $ FiniteBadnessVal Q.hunKInt
      | otherwise -> FiniteCost $ FiniteCostValue (finiteB <> p <> q)

-- If c = ∞ or if p ≤ −10000, Tex seizes the initiative and breaks the page at the best remembered breakpoint.
costImpliesBreakHere :: PageCost -> Bool
costImpliesBreakHere = \case
  InfiniteCost -> True
  FiniteCost finiteCost ->
    finiteCost <= FiniteCostValue (FiniteBadnessVal (invert Q.tenKInt))

runPageBuilder ::
  forall es.
  Log.HexLog :> es =>
  Q.Length ->
  VList ->
  Eff es (Seq Page.Page)
runPageBuilder desiredHeight vList =
  go newCurrentPage vList
  where
    go :: CurrentPage -> VList -> Eff es (Seq Page.Page)
    go currentPage@(CurrentPage curPageElems _bestPointAndCost) toAddElems@(VList toAddElemSeq) =
      case toAddElemSeq of
        Empty ->
          Seq.singleton <$> (setPage desiredHeight curPageElems)
        x :<| xs ->
          let continueToNextElem newPage = go newPage (VList xs)

              currentPageWithAddedElem = currentPage & #items % #unVList %~ (|> x)
           in if not (ListElem.vListContainsBoxes curPageElems)
                then -- If the current vlist has no boxes, we discard a discardable item.
                -- Otherwise, if a discardable item is a legitimate breakpoint, we compute
                -- the cost c of breaking at this point.

                  if vListElemIsDiscardable x
                    then do
                      Log.infoLog $ "Continuing, cos no boxes, with discardable elem: " <> F.sformat ListElem.fmtVListElem x
                      continueToNextElem currentPage
                    else do
                      Log.infoLog $ "Continuing, cos no boxes, with non-discardable elem: " <> F.sformat ListElem.fmtVListElem x
                      continueToNextElem currentPageWithAddedElem
                else case vListElemToBreakItem (currentPageLastElem currentPage, x, seqHeadMay xs) of
                  -- If we can't break here, just add it to the list and continue.
                  Nothing -> do
                    Log.infoLog ("Continuing, non-break item: " <> F.sformat ListElem.fmtVListElem x)
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
                        this <- setPage desiredHeight newPageElems
                        pure $ this <| rest
                      else do
                        -- If the resulting cost <= the smallest cost seen so far, remember
                        -- the current breakpoint as the best so far.
                        Log.infoLog $ "Got non-breakey cost: " <> show cost <> "compare to best: " <> show (currentPageBestCost currentPageWithAddedElem)
                        continueToNextElem (updateWithCost cost currentPageWithAddedElem)
