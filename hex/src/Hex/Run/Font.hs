{-# LANGUAGE OverloadedRecordDot #-}

module Hex.Run.Font where

import Hex.Common.HexState.Interface
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken
import Hex.Run.App
import Hex.Stage.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hexlude

testAppLoadSelectFont :: App ()
testAppLoadSelectFont = do
  fontDef <- loadFont (H.Inter.B.Box.HexFilePath "cmr10.tfm") H.Inter.B.Box.NaturalFont
  selectFont (fontDef ^. typed @FontNumber) Local
