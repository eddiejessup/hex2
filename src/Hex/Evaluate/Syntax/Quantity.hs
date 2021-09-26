{-# LANGUAGE UndecidableInstances #-}

module Hex.Evaluate.Syntax.Quantity where

import Hex.Quantity qualified as H.Q
import Hexlude
import qualified Hex.Syntax.Quantity as H.Syn
import qualified Hex.Syntax.Common as H.Syn

type instance H.Syn.HexPassInt 'H.Syn.Evaluated = H.Q.HexInt
type instance H.Syn.HexPassLength 'H.Syn.Evaluated = H.Q.Length
type instance H.Syn.HexPassMathLength 'H.Syn.Evaluated = H.Q.MathLength
type instance H.Syn.HexPassGlue 'H.Syn.Evaluated = H.Q.Glue
type instance H.Syn.HexPassRegisterIndex 'H.Syn.Evaluated = RegisterIndex

newtype Factor = Factor Rational
  deriving stock (Show, Eq, Generic)

data ExplicitGlueSpec = ExplicitGlueSpec {egLength :: H.Q.Length, egStretch :: Maybe Flex, egShrink :: Maybe Flex}
  deriving stock (Show, Eq, Generic)

data Flex = FiniteFlex H.Q.Length | FilFlex FilLength
  deriving stock (Show, Eq, Generic)

data FilLength = FilLength Factor H.Q.InfLengthOrder
  deriving stock (Show, Eq, Generic)

-- Internal quantities.

newtype RegisterIndex = RegisterIndex H.Q.HexInt
  deriving stock (Show, Eq, Ord, Generic)
