module Hex.Common.HexState.Impl.Defaults.Register where

import Hex.Common.HexState.Impl.Defaults.Common (initialiseFiniteMap)
import Hex.Common.HexState.Interface.Register qualified as Reg
import Hex.Common.HexState.Interface.TokenList qualified as TL
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.BoxElem qualified as BoxElem
import Hexlude

newIntRegister :: Map Reg.RegisterLocation Q.HexInt
newIntRegister = initialiseFiniteMap (const mempty)

newLengthRegister :: Map Reg.RegisterLocation Q.Length
newLengthRegister = initialiseFiniteMap (const mempty)

newGlueRegister :: Map Reg.RegisterLocation Q.Glue
newGlueRegister = initialiseFiniteMap (const mempty)

newMathGlueRegister :: Map Reg.RegisterLocation Q.MathGlue
newMathGlueRegister = initialiseFiniteMap (const mempty)

newTokenListRegister :: Map Reg.RegisterLocation TL.BalancedText
newTokenListRegister = initialiseFiniteMap (const mempty)

-- A box register may actually be empty, it isn't a complete property.
newBoxRegister :: Map Reg.RegisterLocation BoxElem.BaseBox
newBoxRegister = mempty
