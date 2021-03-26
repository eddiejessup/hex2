module Hex.Interpret.Build.List.Horizontal.Evaluate where

import Hex.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Interpret.Build.List.Elem qualified as H.Inter.B.List
import Hex.Interpret.Build.List.Horizontal.Badness
import Hex.Quantity qualified as H.Q
import Hexlude

-- Widths, glue aggregates, box status.

hListNaturalWidth :: H.Inter.B.List.HList -> H.Q.Length
hListNaturalWidth = foldMapOf H.Inter.B.List.hListElemTraversal hListElemNaturalWidth

hListElemNaturalWidth :: H.Inter.B.List.HListElem -> H.Q.Length
hListElemNaturalWidth = \case
  H.Inter.B.List.HVListElem (H.Inter.B.List.ListGlue glue) ->
    glue ^. #gDimen
  H.Inter.B.List.HVListElem (H.Inter.B.List.ListPenalty _) ->
    H.Q.zeroLength
  H.Inter.B.List.HVListElem (H.Inter.B.List.VListBaseElem (H.Inter.B.Box.ElemBox b)) ->
    b ^. #boxWidth
  H.Inter.B.List.HVListElem (H.Inter.B.List.VListBaseElem (H.Inter.B.Box.ElemFontDefinition _)) ->
    H.Q.zeroLength
  H.Inter.B.List.HVListElem (H.Inter.B.List.VListBaseElem (H.Inter.B.Box.ElemFontSelection _)) ->
    H.Q.zeroLength
  H.Inter.B.List.HVListElem (H.Inter.B.List.VListBaseElem (H.Inter.B.Box.ElemKern kern)) ->
    kern ^. #unKern
  H.Inter.B.List.HListHBaseElem (H.Inter.B.Box.ElemCharacter character) ->
    character ^. #unCharacter % #boxWidth

hListNetBiFlex :: H.Inter.B.List.HList -> H.Q.BiNetFlex
hListNetBiFlex = foldOf (H.Inter.B.List.hListElemTraversal % hListElemBiFlex)
  where
    hListElemBiFlex :: AffineFold H.Inter.B.List.HListElem H.Q.BiNetFlex
    hListElemBiFlex = _Typed @H.Inter.B.List.VListElem % _Typed @H.Q.Glue % to H.Q.asBiNetFlex

data GlueFlexSpec = DesiredEqualsNatural | NeedsToFlex ImperfectGlueFlexSpec
  deriving stock (Show, Generic)

data ImperfectGlueFlexSpec = ImperfectGlueFlexSpec FlexDirection FlexSetSpec
  deriving stock (Show, Generic)

data FlexDirection = Stretch | Shrink
  deriving stock (Show, Generic)

data FlexSetSpec = FlexSetSpec FlexSetOrder FlexSetRatio
  deriving stock (Show, Generic)

data FlexSetOrder = FiniteFlexSetOrder | InfFlexSetOrder H.Q.InfLengthOrder
  deriving stock (Show, Generic)

newtype FlexSetRatio = FlexSetRatio {unFlexSetRatio :: Rational}
  deriving stock (Show, Generic)

glueFlexSpec :: H.Q.Length -> H.Q.BiNetFlex -> (GlueFlexSpec, Badness)
glueFlexSpec excessWidth netBiFlex =
  case compare excessWidth H.Q.zeroLength of
    EQ ->
      (DesiredEqualsNatural, zeroBadness)
    -- If x > w, TEX attempts to shrink the contents of the box in a similar
    -- way; the glue order is the highest subscript i such that zi  ̸= 0, and the
    -- glue ratio is normally r = (x−w)/zi. However, r is set to 1.0 in the case
    -- i = 0 and x − w > z0, because the maximum shrinkability must not be
    -- exceeded.
    GT ->
      let netShrinkOrder = netBiFlex ^. #biShrink % to H.Q.highestNetFlexOrder

          (netFlexSetSpec, badness) = case netShrinkOrder of
            H.Q.FinitePureFlex netFiniteShrink ->
              let setRatio =
                    FlexSetRatio $
                      if excessWidth >= netFiniteShrink
                        then 1
                        else H.Q.lengthRatio excessWidth netFiniteShrink

                  setSpec = FlexSetSpec FiniteFlexSetOrder setRatio

                  b = finiteFlexBadness excessWidth netFiniteShrink
               in (setSpec, b)
            H.Q.InfPureFlex (H.Q.InfLengthOfOrder infOrder netNonZeroInfShrink) ->
              let setRatio = FlexSetRatio $ H.Q.lengthRatio excessWidth (netNonZeroInfShrink ^. typed @H.Q.Length)
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

          netStretchOrder = netBiFlex ^. #biStretch % to H.Q.highestNetFlexOrder

          (netFlexSetSpec, badness) = case netStretchOrder of
            H.Q.FinitePureFlex netFiniteStretch ->
              let setRatio =
                    FlexSetRatio $
                      if netFiniteStretch == H.Q.zeroLength
                        then 0
                        else H.Q.lengthRatio lackingWidth netFiniteStretch

                  setSpec = FlexSetSpec FiniteFlexSetOrder setRatio

                  b = finiteFlexBadness excessWidth netFiniteStretch
               in (setSpec, b)
            H.Q.InfPureFlex (H.Q.InfLengthOfOrder infOrder netNonZeroInfStretch) ->
              let setRatio = FlexSetRatio $ H.Q.lengthRatio lackingWidth (netNonZeroInfStretch ^. typed @H.Q.Length)

                  setSpec = FlexSetSpec (InfFlexSetOrder infOrder) setRatio
               in (setSpec, zeroBadness)

          spec = NeedsToFlex $ ImperfectGlueFlexSpec Stretch netFlexSetSpec
       in (spec, badness)

-- Every glob of glue in the horizontal list being boxed is modified.
-- Suppose the glue has natural width u, stretchability y, and shrinkability z,
-- where y is a jth order infinity and z is a kth order infinity. Then if x < w
-- (stretching), this glue takes the new width u+ry if j = i; it keeps its
-- natural width u if j ̸= i. If x > w (shrinking), this glue takes the new
-- width u−rz if k = i; it keeps its natural width u if k ̸= i. Notice that
-- stretching or shrinking occurs only when the glue has the highest order of
-- infinity that doesn’t cancel out.
applyGlueFlexSpec :: GlueFlexSpec -> H.Q.Glue -> H.Inter.B.Box.SetGlue
applyGlueFlexSpec spec g =
  H.Inter.B.Box.SetGlue $
    let thisGlueNaturalWidth = g ^. #gDimen
     in case spec of
          DesiredEqualsNatural ->
            thisGlueNaturalWidth
          NeedsToFlex (ImperfectGlueFlexSpec flexDirection flexSetSpec) ->
            let FlexSetSpec flexSetOrder specSetRatio = flexSetSpec
             in case flexDirection of
                  Stretch ->
                    let thisGlueStretch = g ^. #gStretch
                     in case (flexSetOrder, thisGlueStretch) of
                          -- If we are setting at finite stretch, and this glue itself has finite stretch, then modify.
                          (FiniteFlexSetOrder, H.Q.FinitePureFlex thisFiniteGlueStretch) ->
                            let extraLengthFromStretching = scaleLengthByRatio specSetRatio thisFiniteGlueStretch
                             in thisGlueNaturalWidth <> extraLengthFromStretching
                          -- If we are setting at some infinite stretch, and this glue has finite stretch, then set at the natural width.
                          (InfFlexSetOrder _, H.Q.FinitePureFlex _) ->
                            thisGlueNaturalWidth
                          (InfFlexSetOrder setInfOrder, H.Q.InfPureFlex (H.Q.InfLengthOfOrder thisGlueInfOrder thisInfGlueStretch)) ->
                            if setInfOrder == thisGlueInfOrder
                              then -- If we are setting at some infinite stretch, and this glue has non-zero infinite stretch of the same order, then modify.

                                let extraLengthFromStretching = scaleLengthByRatio specSetRatio (thisInfGlueStretch ^. typed @H.Q.Length)
                                 in thisGlueNaturalWidth <> extraLengthFromStretching
                              else -- If we are setting at some infinite stretch, and this glue has non-zero infinite stretch of the same order, then set at the natural width.

                                thisGlueNaturalWidth
                          -- If we are setting at finite stretch, and this glue has non-zero infinite stretch, then set at the natural width.
                          (FiniteFlexSetOrder, H.Q.InfPureFlex _) ->
                            thisGlueNaturalWidth
                  Shrink ->
                    let thisGlueShrink = g ^. #gShrink
                     in case (flexSetOrder, thisGlueShrink) of
                          -- If we are setting at finite shrink, and this glue itself has finite shrink, then modify.
                          (FiniteFlexSetOrder, H.Q.FinitePureFlex thisFiniteGlueShrink) ->
                            let lesserLengthFromShrinking = scaleLengthByRatio specSetRatio thisFiniteGlueShrink
                             in thisGlueNaturalWidth ~~ lesserLengthFromShrinking
                          -- If we are setting at some infinite shrink, and this glue has finite shrink, then set at the natural width.
                          (InfFlexSetOrder _, H.Q.FinitePureFlex _) ->
                            thisGlueNaturalWidth
                          (InfFlexSetOrder setInfOrder, H.Q.InfPureFlex (H.Q.InfLengthOfOrder thisGlueInfOrder thisInfGlueShrink)) ->
                            if setInfOrder == thisGlueInfOrder
                              then -- If we are setting at some infinite shrink, and this glue has non-zero infinite shrink of the same order, then modify.

                                let lesserLengthFromShrinking = scaleLengthByRatio specSetRatio (thisInfGlueShrink ^. typed @H.Q.Length)
                                 in thisGlueNaturalWidth ~~ lesserLengthFromShrinking
                              else -- If we are setting at some infinite shrink, and this glue has non-zero infinite shrink of the same order, then set at the natural width.

                                thisGlueNaturalWidth
                          -- If we are setting at finite shrink, and this glue has non-zero infinite shrink, then set at the natural width.
                          (FiniteFlexSetOrder, H.Q.InfPureFlex _) ->
                            thisGlueNaturalWidth
  where
    scaleLengthByRatio :: FlexSetRatio -> H.Q.Length -> H.Q.Length
    scaleLengthByRatio (FlexSetRatio r) (H.Q.Length d) =
      let dRational = fromIntegral @Int @Rational d
          scaledRational = r * dRational
       in H.Q.Length $ round scaledRational

listFlexSpec :: H.Inter.B.List.HList -> H.Q.Length -> (GlueFlexSpec, Badness)
listFlexSpec hList desiredWidth =
  let naturalWidth = hListNaturalWidth hList
      excessWidth = naturalWidth ~~ desiredWidth

      biFlex = hListNetBiFlex hList
   in glueFlexSpec excessWidth biFlex
