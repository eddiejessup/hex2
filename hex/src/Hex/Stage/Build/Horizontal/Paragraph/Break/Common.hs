module Hex.Stage.Build.Horizontal.Paragraph.Break.Common where

import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.ListElem (HListElem)
import Hex.Stage.Build.ListElem qualified as ListElem
import Hexlude

data DiscardingState = Discarding | NotDiscarding
  deriving stock (Show, Generic)

prepareHListForBreaking :: ListElem.HList -> ListElem.HList
prepareHListForBreaking (ListElem.HList Empty) =
  ListElem.HList mempty
prepareHListForBreaking (ListElem.HList elems@(elemInit :|> lastElem)) =
  let -- Remove the final item if it's glue.
      trimmedElems = case lastElem of
        ListElem.HVListElem (ListElem.ListGlue _) -> elemInit
        _ -> elems
      -- Add extra bits to finish the list.
      finishedElems = trimmedElems >< finishingElems
   in ListElem.HList finishedElems
  where
    -- \penalty10k \hfil \penalty-10k.
    finishingElems :: Seq HListElem
    finishingElems =
      Empty
        :|> ListElem.HVListElem (ListElem.ListPenalty $ ListElem.Penalty $ Q.HexInt Q.tenK)
        :|> ListElem.HVListElem (ListElem.ListGlue (Q.filStretchGlue Q.bigFilLength))
        :|> ListElem.HVListElem (ListElem.ListPenalty $ ListElem.Penalty $ Q.HexInt $ -Q.tenK)
