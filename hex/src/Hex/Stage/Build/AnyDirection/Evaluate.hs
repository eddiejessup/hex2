module Hex.Stage.Build.AnyDirection.Evaluate where

import Formatting qualified as F
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.AnyDirection.Breaking.Badness qualified as Bad
import Hex.Stage.Build.BoxElem qualified as Box
import Hexlude

data GlueFlexSpec
  = DesiredEqualsNatural
  | NeedsToFlex Bad.GlueFlexProblem
  deriving stock (Show, Generic)

fmtGlueFlexSpec :: Fmt GlueFlexSpec
fmtGlueFlexSpec = F.later $ \case
  DesiredEqualsNatural -> "DesiredEqualsNatural"
  NeedsToFlex problem -> "NeedsToFlex(" <> F.bformat Bad.fmtGlueFlexProblem problem <> ")"

flexSpecIsOverfull :: GlueFlexSpec -> Bool
flexSpecIsOverfull = \case
  DesiredEqualsNatural -> False
  NeedsToFlex imperfectGlueFlexSpec ->
    Bad.glueFlexProblemIsOverfull imperfectGlueFlexSpec

glueFlexSpec :: Q.Length -> Q.BiNetFlex -> GlueFlexSpec
glueFlexSpec excessWidth netBiFlex =
  case compare excessWidth Q.zeroLength of
    EQ ->
      DesiredEqualsNatural
    v ->
      let (dir, flex) = case v of
            GT -> (Q.Shrink, Q.highestNetFlexOrder netBiFlex.biShrink)
            LT -> (Q.Stretch, Q.highestNetFlexOrder netBiFlex.biStretch)
       in NeedsToFlex $ Bad.GlueFlexProblem (Q.FlexInDirection dir flex) excessWidth

glueFlexSpecBadness :: GlueFlexSpec -> Bad.Badness
glueFlexSpecBadness = \case
  DesiredEqualsNatural ->
    Bad.zeroBadness
  NeedsToFlex glueFlexProblem ->
    Bad.glueFlexProblemBadness glueFlexProblem

applyGlueFlexSpec :: GlueFlexSpec -> Q.Glue -> Box.SetGlue
applyGlueFlexSpec spec g =
  let setAt =
        case spec of
          DesiredEqualsNatural ->
            g.gDimen
          NeedsToFlex glueFlexProblem ->
            let setRatio = case glueFlexProblem.flexInDirection.flexDirection of
                  Q.Stretch ->
                    case (glueFlexProblem.flexInDirection.flexAmount, g.gStretch) of
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
                  Q.Shrink ->
                    case (glueFlexProblem.flexInDirection.flexAmount, g.gShrink) of
                      -- If we are setting at finite shrink, and this glue itself has finite shrink, then modify.
                      (Q.FinitePureFlex netFiniteShrink, Q.FinitePureFlex thisFiniteGlueShrink) ->
                        Q.lengthRatio thisFiniteGlueShrink (max glueFlexProblem.excessLength netFiniteShrink)
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
            in g.gDimen ~~ Q.scaleLengthByRational setRatio glueFlexProblem.excessLength
  in Box.SetGlue {setAt, glue = g}
