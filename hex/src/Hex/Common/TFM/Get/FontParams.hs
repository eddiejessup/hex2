module Hex.Common.TFM.Get.FontParams where

import ASCII qualified
import Effectful.Serialize.Get qualified as Get
import Hex.Common.TFM.Get.Common (getDesignSizeLength)
import Hex.Common.TFM.Get.Common qualified as TFM.Get.Common
import Hex.Common.TFM.Types
import Hexlude

readMathSymbolParams :: (Get.Get :> es) => Eff es MathSymbolParams
readMathSymbolParams =
  MathSymbolParams
    <$> getDesignSizeLength
    <*> getDesignSizeLength
    <*> getDesignSizeLength
    <*> getDesignSizeLength
    <*> getDesignSizeLength
    <*> getDesignSizeLength
    <*> getDesignSizeLength
    <*> getDesignSizeLength
    <*> getDesignSizeLength
    <*> getDesignSizeLength
    <*> getDesignSizeLength
    <*> getDesignSizeLength
    <*> getDesignSizeLength
    <*> getDesignSizeLength
    <*> getDesignSizeLength

readMathExtensionParams :: (Get.Get :> es) => Eff es MathExtensionParams
readMathExtensionParams =
  MathExtensionParams
    <$> getDesignSizeLength
    <*> getDesignSizeLength
    <*> getDesignSizeLength
    <*> getDesignSizeLength
    <*> getDesignSizeLength
    <*> getDesignSizeLength

getFontParams :: (Get.Get :> es) => Maybe [ASCII.Char] -> Eff es FontParams
getFontParams scheme =
  FontParams
    <$> TFM.Get.Common.getFixWord
    <*> getDesignSizeLength
    <*> getDesignSizeLength
    <*> getDesignSizeLength
    <*> getDesignSizeLength
    <*> getDesignSizeLength
    <*> getDesignSizeLength
    <*> case ASCII.charListToText <$> scheme of
      Just "TeX math symbols" ->
        Just . MathSymbolFontParams <$> readMathSymbolParams
      Just "TeX math extension" ->
        Just . MathExtensionFontParams <$> readMathExtensionParams
      _ ->
        pure Nothing
