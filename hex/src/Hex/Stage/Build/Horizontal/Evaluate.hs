module Hex.Stage.Build.Horizontal.Evaluate where

import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.AnyDirection.Evaluate qualified as Eval
import Hex.Stage.Build.ListElem qualified as ListElem
import Hexlude

-- Widths, glue aggregates, box status.

hListNetBiFlex :: Seq ListElem.HListElem -> Q.BiNetFlex
hListNetBiFlex = foldOf (traversed % hListElemBiFlex)
  where
    hListElemBiFlex :: AffineFold ListElem.HListElem Q.BiNetFlex
    hListElemBiFlex = _Typed @ListElem.VListElem % _Typed @Q.Glue % to Q.asBiNetFlex

-- If x > w, TEX attempts to shrink the contents of the box in a similar way;
-- the glue order is the highest subscript i such that zi  ̸= 0, and the glue
-- ratio is normally r = (x−w)/zi. However, r is set to 1.0 in the case i = 0
-- and x − w > z0, because the maximum shrinkability must not be exceeded.

-- If x < w, TEX attempts to stretch the contents of the box; the glue order is
-- the highest subscript i such that yi is nonzero, and the glue ratio is r =
-- (w−x)/yi. (If y0 = y1 = y2 = y3 = 0, there’s no stretchability; both i and r
-- are set to zero.)

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

listFlexSpec :: Seq ListElem.HListElem -> Q.Length -> Eval.GlueFlexSpec
listFlexSpec hList desiredWidth =
  Eval.glueFlexSpec ((ListElem.hListNaturalWidth hList) ~~ desiredWidth) (hListNetBiFlex hList)
