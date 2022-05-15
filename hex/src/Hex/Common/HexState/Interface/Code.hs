{-# LANGUAGE TypeFamilyDependencies #-}

module Hex.Common.HexState.Interface.Code where

import Formatting qualified as F
import Hex.Common.Codes qualified as Code
import Hexlude

newtype CodeLocation = CodeLocation {unCodeLocation :: Code.CharCode}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)

fmtCodeLocation :: Fmt CodeLocation
fmtCodeLocation = F.squared (F.accessed (.unCodeLocation) Code.fmtCharCode)

type family CodeTableTarget (c :: Code.CodeType) = result | result -> c where
  CodeTableTarget 'Code.CatCodeType = Code.CatCode
  CodeTableTarget 'Code.MathCodeType = Code.MathCode
  CodeTableTarget 'Code.UpperCaseCodeType = Code.UpperCaseCode
  CodeTableTarget 'Code.LowerCaseCodeType = Code.LowerCaseCode
  CodeTableTarget 'Code.SpaceFactorCodeType = Code.SpaceFactorCode
  CodeTableTarget 'Code.DelimiterCodeType = Code.DelimiterCode
