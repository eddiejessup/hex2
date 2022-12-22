{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Build.ListBuilder.Vertical where

import Data.Sequence qualified as Seq
import Formatting qualified as F
import Hex.Capability.Log.Interface (HexLog (..))
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Box qualified as Box
import Hex.Common.HexState.Interface (EHexState)
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.BoxElem qualified as Box
import Hex.Stage.Build.ListBuilder.Interface
import Hex.Stage.Build.ListElem (VListElem)
import Hex.Stage.Build.ListElem qualified as ListElem
import Hex.Stage.Build.Vertical.Page.Break qualified as V.Break
import Hexlude

addVListElementImpl :: (HSt.EHexState :> es, Log.HexLog :> es, State (Seq VListElem) :> es) => ListElem.VListElem -> Eff es ()
addVListElementImpl e = do
  vList <- get
  newVList <- extendVList e vList
  put newVList

extendVList ::
  (HSt.EHexState :> es, Log.HexLog :> es) =>
  ListElem.VListElem ->
  Seq ListElem.VListElem ->
  Eff es (Seq ListElem.VListElem)
extendVList e accSeq = case e of
  -- TODO: topskip
  ListElem.VListBaseElem (Box.AxOrRuleBoxBaseElem b) -> do
    -- Assume we are adding a non-rule box of height h to the vertical listElem.
    -- Let \prevdepth = p, \lineskiplimit = l, \baselineskip = (b plus y minus z).
    -- Add interline glue, above the new box, of:
    -- If p ≤ −1000 pt:
    --    No glue.
    -- Otherwise, if b−p−h ≥ l:
    --    (b−p−h) plus y minus z
    -- Otherwise:
    --    \lineskip
    -- Then set \prevdepth to the depth of the new box.
    prevDepth <- HSt.getSpecialLengthParameter HSt.Param.PrevDepth
    blineGlue <- HSt.getParameterValue (HSt.Param.GlueQuantParam HSt.Param.BaselineSkip)
    skipLimit <- HSt.getParameterValue (HSt.Param.LengthQuantParam HSt.Param.LineSkipLimit)
    skip <- HSt.getParameterValue (HSt.Param.GlueQuantParam HSt.Param.LineSkip)
    newElems <-
      if prevDepth <= invert Q.oneKPt
        then do
          Log.infoLog $ "extendVList: Adding box element without interline glue because prevDepth is " <> F.sformat Q.fmtLengthWithUnit prevDepth
          pure $ Empty |> e
        else do
          Log.infoLog $ "extendVList: \\glue(\\baselineskip): " <> F.sformat Q.fmtGlue blineGlue
          Log.infoLog $ "extendVList: \\sdimen(\\prevDepth): " <> F.sformat Q.fmtLengthWithUnit prevDepth
          Log.infoLog $ "extendVList: boxHeight:  " <> F.sformat Q.fmtLengthWithUnit b.boxedDims.boxHeight
          let proposedBaselineLength = blineGlue.gDimen ~~ prevDepth ~~ b.boxedDims.boxHeight
          Log.infoLog $ "extendVList: Proposed baseline length: " <> F.sformat Q.fmtLengthWithUnit proposedBaselineLength <> ", skip limit: " <> F.sformat Q.fmtLengthWithUnit skipLimit
          Log.infoLog $ "extendVList: \\dimen(\\lineskiplimit): " <> F.sformat Q.fmtLengthWithUnit skipLimit
          -- Intuition: set the distance between baselines to \baselineskip, but no
          -- closer than \lineskiplimit [theBaselineLengthMin], in which case
          -- \lineskip [theMinBaselineGlue] is used.
          glue <-
            if proposedBaselineLength >= skipLimit
              then do
                let res = blineGlue & #gDimen !~ proposedBaselineLength
                Log.infoLog $ "extendVList: proposedBaselineLength >= skipLimit, so using BaselineSkip with length set to proposedBaselineLength, ie: " <> F.sformat Q.fmtGlue res
                pure res
              else do
                Log.infoLog $ "extendVList: proposedBaselineLength < skipLimit, so using LineSkip ie: " <> F.sformat Q.fmtGlue skip
                pure skip
          Log.infoLog $ "extendVList: Adding interline glue: " <> F.sformat Q.fmtGlue glue
          let glueElem = ListElem.ListGlue glue
          pure $ Empty |> glueElem |> e
    let boxDepth = b.boxedDims.boxDepth
    Log.infoLog $ "extendVList: \\sdimen(\\prevDepth) := " <> F.sformat Q.fmtLengthWithUnit boxDepth
    HSt.setSpecialLengthParameter HSt.Param.PrevDepth boxDepth
    pure $ accSeq <> newElems
  _ -> do
    if not (ListElem.vListContainsBoxes accSeq) && V.Break.vListElemIsDiscardable e
      then do
        Log.infoLog $ "extendVList: Skipping adding element: " <> F.sformat ListElem.fmtVListElem e <> ", to vertical list without boxes, length " <> show (Seq.length accSeq)
        pure accSeq
      else do
        Log.infoLog $ "extendVList: Adding non-box element: " <> F.sformat ListElem.fmtVListElem e
        pure (accSeq :|> e)

runHexListBuilderVMode :: (EHexState :> es, HexLog :> es, State (Seq VListElem) :> es) => Eff (HexListBuilder : es) a -> Eff es a
runHexListBuilderVMode = interpret $ \_ -> \case
  AddVListElement e -> addVListElementImpl e
