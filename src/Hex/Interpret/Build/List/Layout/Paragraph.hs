module Hex.Interpret.Build.List.Layout.Paragraph where

import Protolude
import Hex.Interpret.Evaluate.Evaluated qualified as H.Inter.Eval
import Hex.Interpret.Build.List.Elem qualified as H.Inter.B.List
import Hex.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Data.Sequence qualified as Seq
import Data.Sequence (Seq(..))

layOutParagraph ::
  (Monad m) =>
  H.Inter.Eval.Length -> -- HSize
  H.Inter.Eval.HexInt -> -- Tolerance
  H.Inter.Eval.HexInt -> -- LinePenalty
  H.Inter.B.List.HList ->
  m (Seq (H.Inter.B.Box.Box H.Inter.B.Box.HBox))
layOutParagraph _ _ _ (H.Inter.B.List.HList Seq.Empty) =
  pure mempty
layOutParagraph dw tol lp (H.Inter.B.List.HList (xs :|> x)) = do
  undefined
  -- let -- Remove the final item if it's glue.
  --     trimmedElemSeq = case x of
  --       HVListElem (ListGlue _) -> xs
  --       _ -> xs |> x
  --     -- Add extra bits to finish the list.
  --     -- Append \penalty10k \hfil \penalty-10k.
  --     finishedElemSeq =
  --       trimmedElemSeq
  --         |> HVListElem (ListPenalty $ Penalty tenK)
  --         |> HVListElem (ListGlue filGlue)
  --         |> HVListElem (ListPenalty $ Penalty $ - tenK)

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
