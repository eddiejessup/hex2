module Hex.Stage.Evaluate.Interface.AST.Quantity where

import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hexlude

data CodeTableRef = CodeTableRef {codeTableType :: PT.CodeType, codeTableChar :: Code.CharCode }
  deriving stock (Show, Eq, Generic)
