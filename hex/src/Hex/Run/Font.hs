module Hex.Run.Font where

import Hex.Common.HexState.Interface
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken
import Hex.Common.Quantity.Common qualified as Q
import Hex.Run.App
import Hex.Stage.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hexlude

testAppLoadSelectFont :: App ()
testAppLoadSelectFont = do
  fontDef <- loadFont (Q.HexFilePath "cmr10.tfm") H.Inter.B.Box.NaturalFont
  selectFont (fontDef ^. typed @FontNumber) Local
