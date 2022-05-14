{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Evaluate.Interface.AST.Quantity where

import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.HexState.Interface.TokenList qualified as TL
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Parse.Interface.AST.Quantity qualified as Uneval
import Hexlude

data CodeTableRef = CodeTableRef {codeTableType :: PT.CodeType, codeTableChar :: Code.CharCode}
  deriving stock (Show, Eq, Generic)

data InternalQuantity
  = InternalIntQuantity Q.HexInt
  | InternalLengthQuantity Q.Length
  | InternalGlueQuantity Q.Glue
  | InternalMathGlueQuantity Q.MathGlue
  | FontQuantity Uneval.FontRef
  | TokenListVariableQuantity TL.BalancedText
  deriving stock (Show, Eq, Generic)
