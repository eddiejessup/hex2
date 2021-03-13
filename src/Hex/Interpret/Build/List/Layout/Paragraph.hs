module Hex.Interpret.Build.List.Layout.Paragraph where

import Data.Ratio qualified as Ratio
import Hex.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Interpret.Build.List.Elem qualified as H.Inter.B.List
import Hex.Quantity qualified as H.Q
import Hexlude

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

data BreakItem
  = GlueBreak H.Q.Glue
  | KernBreak H.Inter.B.Box.Kern
  | PenaltyBreak H.Inter.B.List.Penalty
  | NoBreak
  deriving stock (Show, Generic)

vListElemIsDiscardable :: H.Inter.B.List.VListElem -> Bool
vListElemIsDiscardable = \case
  H.Inter.B.List.ListGlue _ -> True
  H.Inter.B.List.ListPenalty _ -> True
  H.Inter.B.List.VListBaseElem (H.Inter.B.Box.ElemKern _) -> True
  _ -> False

hListElemisDiscardable :: H.Inter.B.List.HListElem -> Bool
hListElemisDiscardable (H.Inter.B.List.HVListElem e) = vListElemIsDiscardable e
hListElemisDiscardable (H.Inter.B.List.HListHBaseElem (H.Inter.B.Box.ElemCharacter _)) = False

-- TODO: Add math formula conditions.
-- TODO: Discretionary break and Math-off.
hListElemToBreakItem :: (Maybe H.Inter.B.List.HListElem, H.Inter.B.List.HListElem, Maybe H.Inter.B.List.HListElem) -> Maybe BreakItem
hListElemToBreakItem = \case
  (Just x, H.Inter.B.List.HVListElem (H.Inter.B.List.ListGlue g), _)
    | not (hListElemisDiscardable x) ->
      Just $ GlueBreak g
  (_, H.Inter.B.List.HVListElem (H.Inter.B.List.VListBaseElem (H.Inter.B.Box.ElemKern k)), Just (H.Inter.B.List.HVListElem (H.Inter.B.List.ListGlue _))) ->
    Just $ KernBreak k
  (_, H.Inter.B.List.HVListElem (H.Inter.B.List.ListPenalty p), _) ->
    Just $ PenaltyBreak p
  _ -> Nothing

-- \penalty10k \hfil \penalty-10k.
finishingHListElems :: Seq H.Inter.B.List.HListElem
finishingHListElems =
  Empty
    :|> H.Inter.B.List.HVListElem (H.Inter.B.List.ListPenalty $ H.Inter.B.List.Penalty H.Q.tenK)
    :|> H.Inter.B.List.HVListElem (H.Inter.B.List.ListGlue H.Q.filGlue)
    :|> H.Inter.B.List.HVListElem (H.Inter.B.List.ListPenalty $ H.Inter.B.List.Penalty $ -H.Q.tenK)

prepareHListForLayout :: H.Inter.B.List.HList -> Seq (H.Inter.B.List.HListElem, Maybe BreakItem)
prepareHListForLayout (H.Inter.B.List.HList Empty) =
  mempty
prepareHListForLayout (H.Inter.B.List.HList hListElems@(hListElemInit :|> lastHListElem)) =
  let -- Remove the final item if it's glue.
      trimmedHListElems = case lastHListElem of
        H.Inter.B.List.HVListElem (H.Inter.B.List.ListGlue _) -> hListElemInit
        _ -> hListElems
      -- Add extra bits to finish the list.
      finishedHListElems = trimmedHListElems >< finishingHListElems
      adjs = toAdjacents finishedHListElems
      elemsWithBreaks = adjs <&> \adj@(_, e, _) -> (e, hListElemToBreakItem adj)
   in elemsWithBreaks

allFirstLines :: Seq (a, Maybe BreakItem) -> Seq (Seq a, Maybe BreakItem)
allFirstLines es = snd (foldl' f (Empty, Empty) es) :|> (fst <$> es, Nothing)
  where
    f (bef, res) (x, mayB) = case mayB of
      Nothing -> (bef :|> x, res)
      Just b -> (bef :|> x, res :|> (bef, Just b))

newtype Badness = Badness {unBadness :: Int}

badness :: H.Q.Length -> H.Q.Length -> Badness
badness requiredFlex flexibility =
  let lenToInteger = view (typed @Int % to (fromIntegral @Int @Integer))
      flexRatio = lenToInteger requiredFlex Ratio.% lenToInteger flexibility
   in Badness $ min H.Q.tenK $ round $ (flexRatio ^ (3 :: Int)) * 100

layOutParagraph ::
  (Monad m) =>
  H.Q.Length -> -- HSize
  H.Q.HexInt -> -- Tolerance
  H.Q.HexInt -> -- LinePenalty
  H.Inter.B.List.HList ->
  m (Seq (H.Inter.B.Box.Box H.Inter.B.Box.HBox))
layOutParagraph _ _ _ (H.Inter.B.List.HList Empty) =
  pure mempty
layOutParagraph dw tol lp hList = do
  let elemsWithBreaks = prepareHListForLayout hList
  panic "not implemented"

-- BreakingState {accEdges, nodeToBestRoute, chunk} <-
--   execStateT
--     (forM_ (Adj.toAdjacents finishedElemSeq) (appendEntry dw tol lp))
--     initialBreakingState
-- let inEdges = inEdgeConcat chunk <$> accEdges
--     acceptableDInEdges = toOnlyAcceptables tol lp NoBreak $ withTarget dw <$> inEdges
-- setRoute <$> bestRoute acceptableDInEdges nodeToBestRoute
-- where
--   setRoute route =
--     (\(tgt, hList) -> setHList hList (ComputedTargetLength tgt)) <$> routeSolution route
