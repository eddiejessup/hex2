module Hex.Stage.Evaluate.Interface.AST.Common where

import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as H.Sym.Tok
import Hexlude

data CodeTableRef = CodeTableRef H.Sym.Tok.CodeType Code.CharCode
  deriving stock (Show, Eq, Generic)
