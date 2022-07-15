module Hex.Common.HexState.Impl.Defaults.Register where

import Hex.Common.Quantity qualified as Q
import Hexlude
import Hex.Common.HexState.Impl.Defaults.Common (initialiseFiniteMap)
import qualified Hex.Common.HexState.Interface.Register as Reg
import qualified Hex.Stage.Build.BoxElem as Box
import qualified Hex.Common.HexState.Interface.TokenList as TL

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
newBoxRegister :: Map Reg.RegisterLocation (Box.Box Box.BaseBoxContents)
newBoxRegister = mempty
