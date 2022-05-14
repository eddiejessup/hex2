module Hex.Stage.Interpret.Build.List.Horizontal.Evaluate where

import Hex.Common.Quantity qualified as Q
import Hex.Stage.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Stage.Interpret.Build.List.Elem qualified as H.Inter.B.List
import Hex.Stage.Interpret.Build.List.Horizontal.Badness
import Hexlude

-- Widths, glue aggregates, box status.

hListNaturalWidth :: H.Inter.B.List.HList -> Q.Length
hListNaturalWidth = foldMapOf H.Inter.B.List.hListElemTraversal hListElemNaturalWidth

hListElemNaturalWidth :: H.Inter.B.List.HListElem -> Q.Length
hListElemNaturalWidth = \case
  H.Inter.B.List.HVListElem (H.Inter.B.List.ListGlue glue) ->
    glue ^. #gDimen
  H.Inter.B.List.HVListElem (H.Inter.B.List.ListPenalty _) ->
    Q.zeroLength
  H.Inter.B.List.HVListElem (H.Inter.B.List.VListBaseElem (H.Inter.B.Box.ElemBox b)) ->
    b ^. #boxWidth
  H.Inter.B.List.HVListElem (H.Inter.B.List.VListBaseElem (H.Inter.B.Box.ElemFontDefinition _)) ->
    Q.zeroLength
  H.Inter.B.List.HVListElem (H.Inter.B.List.VListBaseElem (H.Inter.B.Box.ElemFontSelection _)) ->
    Q.zeroLength
  H.Inter.B.List.HVListElem (H.Inter.B.List.VListBaseElem (H.Inter.B.Box.ElemKern kern)) ->
    kern ^. typed @Q.Length
  H.Inter.B.List.HListHBaseElem (H.Inter.B.Box.ElemCharacter character) ->
    character ^. #unCharacter % #boxWidth

hListNetBiFlex :: H.Inter.B.List.HList -> Q.BiNetFlex
hListNetBiFlex = foldOf (H.Inter.B.List.hListElemTraversal % hListElemBiFlex)
  where
    hListElemBiFlex :: AffineFold H.Inter.B.List.HListElem Q.BiNetFlex
    hListElemBiFlex = _Typed @H.Inter.B.List.VListElem % _Typed @Q.Glue % to Q.asBiNetFlex

data GlueFlexSpec = DesiredEqualsNatural | NeedsToFlex ImperfectGlueFlexSpec
  deriving stock (Show, Generic)

data ImperfectGlueFlexSpec = ImperfectGlueFlexSpec FlexDirection FlexSetSpec
  deriving stock (Show, Generic)

data FlexDirection = Stretch | Shrink
  deriving stock (Show, Generic)

data FlexSetSpec = FlexSetSpec FlexSetOrder FlexSetRatio
  deriving stock (Show, Generic)

data FlexSetOrder = FiniteFlexSetOrder | InfFlexSetOrder Q.InfFlexOrder
  deriving stock (Show, Generic)

newtype FlexSetRatio = FlexSetRatio {unFlexSetRatio :: Rational}
  deriving stock (Show, Generic)

glueFlexSpec :: Q.Length -> Q.BiNetFlex -> (GlueFlexSpec, Badness)
glueFlexSpec excessWidth netBiFlex =
  case compare excessWidth Q.zeroLength of
    EQ ->
      (DesiredEqualsNatural, zeroBadness)
    -- If x > w, TEX attempts to shrink the contents of the box in a similar
    -- way; the glue order is the highest subscript i such that zi  ̸= 0, and the
    -- glue ratio is normally r = (x−w)/zi. However, r is set to 1.0 in the case
    -- i = 0 and x − w > z0, because the maximum shrinkability must not be
    -- exceeded.
    GT ->
      let netShrinkOrder = netBiFlex ^. #biShrink % to Q.highestNetFlexOrder

          (netFlexSetSpec, badness) = case netShrinkOrder of
            Q.FinitePureFlex netFiniteShrink ->
              let setRatio =
                    FlexSetRatio $
                      if excessWidth >= netFiniteShrink
                        then 1
                        else Q.lengthRatio excessWidth netFiniteShrink

                  setSpec = FlexSetSpec FiniteFlexSetOrder setRatio

                  b = finiteFlexBadness excessWidth netFiniteShrink
               in (setSpec, b)
            Q.InfPureFlex (Q.InfFlexOfOrder netNonZeroInfShrink infOrder) ->
              let setRatio = FlexSetRatio $ infLengthRatio excessWidth netNonZeroInfShrink
                  setSpec = FlexSetSpec (InfFlexSetOrder infOrder) setRatio
               in (setSpec, zeroBadness)
          spec = NeedsToFlex $ ImperfectGlueFlexSpec Shrink netFlexSetSpec
       in (spec, badness)
    -- If x < w, TEX attempts to stretch the contents of the box; the glue order
    -- is the highest subscript i such that yi is nonzero, and the glue ratio is
    -- r = (w−x)/yi. (If y0 = y1 = y2 = y3 = 0, there’s no stretchability; both
    -- i and r are set to zero.)
    LT ->
      let lackingWidth = invert excessWidth

          netStretchOrder = netBiFlex ^. #biStretch % to Q.highestNetFlexOrder

          (netFlexSetSpec, badness) = case netStretchOrder of
            Q.FinitePureFlex netFiniteStretch ->
              let setRatio =
                    FlexSetRatio $
                      if netFiniteStretch == Q.zeroLength
                        then 0
                        else Q.lengthRatio lackingWidth netFiniteStretch

                  setSpec = FlexSetSpec FiniteFlexSetOrder setRatio

                  b = finiteFlexBadness excessWidth netFiniteStretch
               in (setSpec, b)
            Q.InfPureFlex (Q.InfFlexOfOrder netNonZeroInfStretch infOrder) ->
              let setRatio = FlexSetRatio $ infLengthRatio lackingWidth netNonZeroInfStretch

                  setSpec = FlexSetSpec (InfFlexSetOrder infOrder) setRatio
               in (setSpec, zeroBadness)

          spec = NeedsToFlex $ ImperfectGlueFlexSpec Stretch netFlexSetSpec
       in (spec, badness)
  where
    infLengthRatio = notImplemented "infLengthRatio"

-- Every glob of glue in the horizontal list being boxed is modified.
-- Suppose the glue has natural width u, stretchability y, and shrinkability z,
-- where y is a jth order infinity and z is a kth order infinity. Then if x < w
-- (stretching), this glue takes the new width u+ry if j = i; it keeps its
-- natural width u if j ̸= i. If x > w (shrinking), this glue takes the new
-- width u−rz if k = i; it keeps its natural width u if k ̸= i. Notice that
-- stretching or shrinking occurs only when the glue has the highest order of
-- infinity that doesn’t cancel out.
applyGlueFlexSpec :: GlueFlexSpec -> Q.Glue -> H.Inter.B.Box.SetGlue
applyGlueFlexSpec _spec _g = notImplemented "applyGlueFlexSpec"

-- H.Inter.B.Box.SetGlue $
--   let thisGlueNaturalWidth = g ^. #gDimen
--    in case spec of
--         DesiredEqualsNatural ->
--           thisGlueNaturalWidth
--         NeedsToFlex (ImperfectGlueFlexSpec flexDirection flexSetSpec) ->
--           let FlexSetSpec flexSetOrder specSetRatio = flexSetSpec
--            in case flexDirection of
--                 Stretch ->
--                   let thisGlueStretch = g ^. #gStretch
--                    in case (flexSetOrder, thisGlueStretch) of
--                         -- If we are setting at finite stretch, and this glue itself has finite stretch, then modify.
--                         (FiniteFlexSetOrder, Q.FinitePureFlex thisFiniteGlueStretch) ->
--                           let extraLengthFromStretching = scaleLengthByRatio specSetRatio thisFiniteGlueStretch
--                            in thisGlueNaturalWidth <> extraLengthFromStretching
--                         -- If we are setting at some infinite stretch, and this glue has finite stretch, then set at the natural width.
--                         (InfFlexSetOrder _, Q.FinitePureFlex _) ->
--                           thisGlueNaturalWidth
--                         (InfFlexSetOrder setInfOrder, Q.InfPureFlex (Q.InfFlexOfOrder thisInfGlueStretch thisGlueInfOrder)) ->
--                           if setInfOrder == thisGlueInfOrder
--                             then -- If we are setting at some infinite stretch, and this glue has non-zero infinite stretch of the same order, then modify.

--                               let extraLengthFromStretching = scaleInfLengthByRatio specSetRatio thisInfGlueStretch
--                                in thisGlueNaturalWidth <> extraLengthFromStretching
--                             else -- If we are setting at some infinite stretch, and this glue has non-zero infinite stretch of the same order, then set at the natural width.
--                               thisGlueNaturalWidth
--                         -- If we are setting at finite stretch, and this glue has non-zero infinite stretch, then set at the natural width.
--                         (FiniteFlexSetOrder, Q.InfPureFlex _) ->
--                           thisGlueNaturalWidth
--                 Shrink ->
--                   let thisGlueShrink = g ^. #gShrink
--                    in case (flexSetOrder, thisGlueShrink) of
--                         -- If we are setting at finite shrink, and this glue itself has finite shrink, then modify.
--                         (FiniteFlexSetOrder, Q.FinitePureFlex thisFiniteGlueShrink) ->
--                           let lesserLengthFromShrinking = scaleLengthByRatio specSetRatio thisFiniteGlueShrink
--                            in thisGlueNaturalWidth ~~ lesserLengthFromShrinking
--                         -- If we are setting at some infinite shrink, and this glue has finite shrink, then set at the natural width.
--                         (InfFlexSetOrder _, Q.FinitePureFlex _) ->
--                           thisGlueNaturalWidth
--                         (InfFlexSetOrder setInfOrder, Q.InfPureFlex (Q.InfFlexOfOrder thisInfGlueShrink thisGlueInfOrder)) ->
--                           if setInfOrder == thisGlueInfOrder
--                             then -- If we are setting at some infinite shrink, and this glue has non-zero infinite shrink of the same order, then modify.

--                               let lesserLengthFromShrinking = scaleInfLengthByRatio specSetRatio thisInfGlueShrink
--                                in thisGlueNaturalWidth ~~ lesserLengthFromShrinking
--                             else -- If we are setting at some infinite shrink, and this glue has non-zero infinite shrink of the same order, then set at the natural width.
--                               thisGlueNaturalWidth
--                         -- If we are setting at finite shrink, and this glue has non-zero infinite shrink, then set at the natural width.
--                         (FiniteFlexSetOrder, Q.InfPureFlex _) ->
--                           thisGlueNaturalWidth
-- where
--   scaleLengthByRatio :: FlexSetRatio -> Q.Length -> Q.Length
--   scaleLengthByRatio (FlexSetRatio r) = Q.scaleLengthByRational r

-- scaleInfLengthByRatio :: FlexSetRatio -> Q.InfLength -> Q.InfLength
-- scaleInfLengthByRatio (FlexSetRatio r) = Q.scaleInfLengthByRational r

listFlexSpec :: H.Inter.B.List.HList -> Q.Length -> (GlueFlexSpec, Badness)
listFlexSpec hList desiredWidth =
  let naturalWidth = hListNaturalWidth hList
      excessWidth = naturalWidth ~~ desiredWidth

      biFlex = hListNetBiFlex hList
   in glueFlexSpec excessWidth biFlex
