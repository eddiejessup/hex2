{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Evaluate.Interface.AST.Quantity where

import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Font qualified as HSt.Font
import Hex.Common.HexState.Interface.TokenList qualified as TL
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Parse.Interface.AST.Quantity qualified as Uneval
import Hex.Stage.Render.Interface.DocInstruction qualified as DVI
import Hexlude

data CodeTableRef = CodeTableRef {codeTableType :: Code.CodeType, codeTableChar :: Code.CharCode}
  deriving stock (Show, Eq, Generic)

data CCodeTableRef c = CCodeTableRef {ccodeTableType :: Code.CCodeType c, codeTableChar :: Code.CharCode}
  deriving stock (Show, Eq, Generic)

data InternalQuantity
  = InternalIntQuantity Q.HexInt
  | InternalLengthQuantity Q.Length
  | InternalGlueQuantity Q.Glue
  | InternalMathGlueQuantity Q.MathGlue
  | FontQuantity Uneval.FontRef
  | TokenListVariableQuantity TL.BalancedText
  deriving stock (Show, Eq, Generic)

data FontSpecialCharRef = FontSpecialCharRef HSt.Font.FontSpecialChar DVI.FontNumber
  deriving stock (Show, Eq, Generic)
