module Hex.Stage.Build.Horizontal.Paragraph.Break.Common where

import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.AnyDirection.Breaking.Badness qualified as Bad
import Hex.Stage.Build.ListElem (HListElem)
import Hex.Stage.Build.ListElem qualified as ListElem
import Hexlude

data DiscardingState = Discarding | NotDiscarding
  deriving stock (Show, Generic)

data LineBreakingEnv = LineBreakingEnv
  { hSize,
    emergencyStretch ::
      Q.Length,
    tolerance,
    hyphenPenalty,
    exHyphenPenalty,
    linePenalty ::
      Bad.FiniteBadnessVal,
    leftSkip,
    rightSkip ::
      Q.Glue
  }
  deriving stock (Show, Generic)

mkLineBreakingEnv ::
  Q.Length -> -- HSize
  Q.Length -> -- Emergency stretch
  Q.HexInt -> -- hyphenpenalty
  Q.HexInt -> -- exhyphenpenalty
  Q.HexInt -> -- Tolerance
  Q.HexInt -> -- LinePenalty
  Q.Glue -> -- LeftSkip
  Q.Glue -> -- RightSkip
  LineBreakingEnv
mkLineBreakingEnv
  hSize
  emergencyStretch
  hyphenPenalty
  exHyphenPenalty
  tolerance
  linePenalty
  leftSkip
  rightSkip =
    LineBreakingEnv
      { hSize,
        emergencyStretch,
        tolerance = Bad.FiniteBadnessVal tolerance,
        hyphenPenalty = Bad.FiniteBadnessVal hyphenPenalty,
        exHyphenPenalty = Bad.FiniteBadnessVal exHyphenPenalty,
        linePenalty = Bad.FiniteBadnessVal linePenalty,
        leftSkip,
        rightSkip
      }

prepareHListForBreaking :: Seq HListElem -> Seq HListElem
prepareHListForBreaking = \case
  Empty ->
    mempty
  elems ->
    let -- Remove the final item if it's glue.
        trimmedElems = case elems of
          elemInit :|> ListElem.HVListElem (ListElem.ListGlue _) -> elemInit
          _ -> elems
     in -- Add extra bits to finish the list.
        -- \penalty10k \hfil \penalty-10k.

        trimmedElems
          |> ListElem.HVListElem (ListElem.ListPenalty Bad.maxFiniteBadness)
          |> ListElem.HVListElem (ListElem.ListGlue (Q.filStretchGlue Q.bigFilLength))
          |> ListElem.HVListElem (ListElem.ListPenalty $ invert Bad.maxFiniteBadness)
