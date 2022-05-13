{-# LANGUAGE UndecidableInstances #-}
module Hex.Stage.Evaluate.Interface.AST.Quantity where

import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Impl.Scoped.Scope qualified as Scope
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as ST
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Parse.Interface.AST.Quantity qualified as Uneval
import Hexlude

data CodeTableRef = CodeTableRef {codeTableType :: PT.CodeType, codeTableChar :: Code.CharCode}
  deriving stock (Show, Eq, Generic)

type family QuantVariableTargetEval a where
  QuantVariableTargetEval 'PT.IntQuantity = Q.HexInt
  QuantVariableTargetEval 'PT.LengthQuantity = Q.Length
  QuantVariableTargetEval 'PT.GlueQuantity = Q.Glue
  QuantVariableTargetEval 'PT.MathGlueQuantity = Q.MathGlue
  QuantVariableTargetEval 'PT.TokenListQuantity = TokenListAssignmentTarget

newtype TokenListAssignmentTarget = TokenListAssignmentTarget ST.InhibitedBalancedText
  deriving stock (Show, Eq, Generic)

data QuantVariableEval (a :: PT.QuantityType) = ParamVar (Uneval.QuantParam a) | RegisterVar Scope.RegisterLocation
  deriving stock (Generic)

deriving stock instance Show (Uneval.QuantParam a) => Show (QuantVariableEval a)

deriving stock instance Eq (Uneval.QuantParam a) => Eq (QuantVariableEval a)

data InternalQuantity
  = InternalIntQuantity Q.HexInt
  | InternalLengthQuantity Q.Length
  | InternalGlueQuantity Q.Glue
  | InternalMathGlueQuantity Q.MathGlue
  | FontQuantity Uneval.FontRef
  | TokenListVariableQuantity TokenListAssignmentTarget
  deriving stock (Show, Eq, Generic)
