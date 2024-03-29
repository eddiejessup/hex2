module Hex.Common.HexState.Interface.Mode where

import Formatting qualified as F
import Hexlude

data ModeWithVariant = ModeWithVariant
  { mode :: Mode,
    variant :: ModeVariant
  }

fmtModeWithVariant :: Fmt ModeWithVariant
fmtModeWithVariant = F.later $ \(ModeWithVariant md var) -> case (md, var) of
  (HorizontalMode, OuterModeVariant) -> "Outer H Mode"
  (VerticalMode, OuterModeVariant) -> "Outer V Mode"
  (MathMode, OuterModeVariant) -> "Outer Math Mode"
  (HorizontalMode, InnerModeVariant) -> "Inner H Mode"
  (VerticalMode, InnerModeVariant) -> "Inner V Mode"
  (MathMode, InnerModeVariant) -> "Inner Math Mode"

data Mode = HorizontalMode | VerticalMode | MathMode
  deriving stock (Show, Eq, Generic)

data ModeVariant = OuterModeVariant | InnerModeVariant
  deriving stock (Show, Eq, Generic)

data ModeAttribute
  = InMode Mode
  | InInnerMode
  deriving stock (Show, Eq, Generic)

modeAttributeHolds :: ModeAttribute -> ModeWithVariant -> Bool
modeAttributeHolds (InMode askMode) (ModeWithVariant mode _) = askMode == mode
modeAttributeHolds InInnerMode (ModeWithVariant _ var) = case var of
  InnerModeVariant -> True
  OuterModeVariant -> False

data NonMainVMode
  = InnerVMode
  | OuterHMode
  | InnerHMode
  | OuterMathMode
  | InnerMathMode

hModeFromVariant :: ModeVariant -> NonMainVMode
hModeFromVariant OuterModeVariant = OuterHMode
hModeFromVariant InnerModeVariant = InnerHMode

vModeFromVariant :: ModeVariant -> Maybe NonMainVMode
vModeFromVariant OuterModeVariant = Nothing
vModeFromVariant InnerModeVariant = Just InnerVMode

asModeWithVariant :: NonMainVMode -> ModeWithVariant
asModeWithVariant = \case
  InnerVMode -> ModeWithVariant VerticalMode InnerModeVariant
  OuterHMode -> ModeWithVariant HorizontalMode OuterModeVariant
  InnerHMode -> ModeWithVariant HorizontalMode InnerModeVariant
  OuterMathMode -> ModeWithVariant MathMode OuterModeVariant
  InnerMathMode -> ModeWithVariant MathMode InnerModeVariant
