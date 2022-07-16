module Hex.Stage.Build.Horizontal.Evaluate where

import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.BoxElem qualified as H.Inter.B.Box
import Hex.Stage.Build.Horizontal.Badness
import Hex.Stage.Build.ListElem qualified as H.Inter.B.List
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

data ImperfectGlueFlexSpec = ImperfectGlueFlexSpec {flexDirection :: FlexDirection, netFlexInDirection :: Q.PureFlex, excessWidth :: Q.Length}
  deriving stock (Show, Generic)

data FlexDirection = Stretch | Shrink
  deriving stock (Show, Generic)

-- If x > w, TEX attempts to shrink the contents of the box in a similar way;
-- the glue order is the highest subscript i such that zi  ̸= 0, and the glue
-- ratio is normally r = (x−w)/zi. However, r is set to 1.0 in the case i = 0
-- and x − w > z0, because the maximum shrinkability must not be exceeded.

-- If x < w, TEX attempts to stretch the contents of the box; the glue order is
-- the highest subscript i such that yi is nonzero, and the glue ratio is r =
-- (w−x)/yi. (If y0 = y1 = y2 = y3 = 0, there’s no stretchability; both i and r
-- are set to zero.)

glueFlexSpec :: Q.Length -> Q.BiNetFlex -> (GlueFlexSpec, Badness)
glueFlexSpec excessWidth netBiFlex =
  case compare excessWidth Q.zeroLength of
    EQ ->
      (DesiredEqualsNatural, zeroBadness)
    v ->
      let (dir, flex) = case v of
            GT -> (Shrink, Q.highestNetFlexOrder netBiFlex.biShrink)
            LT -> (Stretch, Q.highestNetFlexOrder netBiFlex.biStretch)
       in (NeedsToFlex $ ImperfectGlueFlexSpec dir flex excessWidth, flexBadness flex)
  where
    flexBadness :: Q.PureFlex -> Badness
    flexBadness = \case
      Q.FinitePureFlex netFiniteFlex ->
        finiteFlexBadness excessWidth netFiniteFlex
      Q.InfPureFlex _ ->
        zeroBadness

-- >>> finiteFlexBadness (Q.pt 1) (Q.pt 1)
-- Badness_ {unBadness = 100}
-- >>> finiteFlexBadness (Q.pt 1) (Q.pt 2)
-- Badness_ {unBadness = 12}
-- >>> finiteFlexBadness (Q.pt 1) (Q.pt 0)
-- Badness_ {unBadness = 10000}

-- Every glob of glue in the horizontal list being boxed is modified.
-- Suppose the glue has natural width u, stretchability y, and shrinkability z,
-- where y is a jth order infinity and z is a kth order infinity.
-- We are calculating the set-length of the glue, 'w'.

-- x < w                       (stretching)
-- =====
--   j = i:  w = u + ry        (glue is stretching and has relevant order)
--   j /= i: w = u             (glue is stretching but has non-relevant order)

-- x > w                       (shrinking)
-- =====
--  k = i: w = u − rz          (glue is shrinking and has relevant order)
--  k /= i: w = u              (glue is shrinking but has non-relevant order)

-- Note that stretching or shrinking occurs only when the glue has the highest
-- order of infinity that doesn’t cancel out.

-- >>> applyGlueFlexSpec (DesiredEqualsNatural) Q.zeroGlue
-- SetGlue {sgDimen = Length {unLength = HexInt {unHexInt = 0}}}
-- >>> applyGlueFlexSpec (NeedsToFlex (ImperfectGlueFlexSpec {flexDirection = Stretch, netFlexInDirection = Q.FinitePureFlex (Q.pt 1), excessWidth = (Q.pt 1)})) (Q.Glue (Q.pt 1) (Q.finFlex (Q.pt 1)) (Q.finFlex (Q.pt 1)))
-- SetGlue {sgDimen = Length {unLength = HexInt {unHexInt = 131072}}}
-- >>> applyGlueFlexSpec (NeedsToFlex (ImperfectGlueFlexSpec {flexDirection = Shrink, netFlexInDirection = Q.FinitePureFlex (Q.pt 1), excessWidth = (Q.pt 1)})) (Q.Glue (Q.pt 1) (Q.finFlex (Q.pt 1)) (Q.finFlex (Q.pt 1)))
-- SetGlue {sgDimen = Length {unLength = HexInt {unHexInt = 0}}}

applyGlueFlexSpec :: GlueFlexSpec -> Q.Glue -> H.Inter.B.Box.SetGlue
applyGlueFlexSpec spec g =
  case spec of
    DesiredEqualsNatural ->
      H.Inter.B.Box.SetGlue $ g.gDimen
    NeedsToFlex imperfectGlueFlexSpec ->
      let setRatio = case imperfectGlueFlexSpec.flexDirection of
            Stretch ->
              case (imperfectGlueFlexSpec.netFlexInDirection, g.gStretch) of
                (Q.FinitePureFlex netFiniteStretch, Q.FinitePureFlex thisFiniteGlueStretch) ->
                  if netFiniteStretch == Q.zeroLength then 0 else Q.lengthRatio thisFiniteGlueStretch netFiniteStretch
                (Q.InfPureFlex (Q.InfFlexOfOrder netInfStretch setInfOrder), Q.InfPureFlex (Q.InfFlexOfOrder thisInfGlueStretch thisGlueInfOrder))
                  | setInfOrder == thisGlueInfOrder ->
                      Q.infLengthRatio thisInfGlueStretch netInfStretch
                -- If we are setting at finite stretch, and this glue has
                -- non-zero infinite stretch, then set at the natural
                -- width.
                -- If we are setting at some infinite stretch, and this
                -- glue has finite stretch, then set at the natural width.
                _ ->
                  0
            Shrink ->
              negate $ case (imperfectGlueFlexSpec.netFlexInDirection, g.gShrink) of
                -- If we are setting at finite shrink, and this glue itself has finite shrink, then modify.
                (Q.FinitePureFlex netFiniteShrink, Q.FinitePureFlex thisFiniteGlueShrink) ->
                  if imperfectGlueFlexSpec.excessWidth >= netFiniteShrink then 1 else Q.lengthRatio thisFiniteGlueShrink netFiniteShrink
                -- If we are setting at some infinite shrink, and this glue has finite shrink, then set at the natural width.
                (Q.InfPureFlex (Q.InfFlexOfOrder netInfShrink setInfOrder), Q.InfPureFlex (Q.InfFlexOfOrder thisInfGlueShrink thisGlueInfOrder))
                  | setInfOrder == thisGlueInfOrder ->
                      -- If we are setting at some infinite shrink, and this
                      -- glue has non-zero infinite shrink of the same order,
                      -- then modify.
                      Q.infLengthRatio thisInfGlueShrink netInfShrink
                -- If we are setting at finite shrink, and this glue has
                -- non-zero infinite shrink, then set at the natural
                -- width.
                -- If we are setting at some infinite shrink, and this
                -- glue has non-zero infinite shrink of a different order,
                -- then set at the natural width.
                _ ->
                  0
       in H.Inter.B.Box.SetGlue $ g.gDimen <> Q.scaleLengthByRational setRatio imperfectGlueFlexSpec.excessWidth

listFlexSpec :: H.Inter.B.List.HList -> Q.Length -> (GlueFlexSpec, Badness)
listFlexSpec hList desiredWidth =
   glueFlexSpec ((hListNaturalWidth hList) ~~ desiredWidth) (hListNetBiFlex hList)
