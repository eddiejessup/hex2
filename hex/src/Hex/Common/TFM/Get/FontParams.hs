module Hex.Common.TFM.Get.FontParams where

import ASCII qualified
import Data.Serialize.Get qualified as Ser
import Hex.Common.TFM.Get.Common qualified as TFM.Get.Common
import Hex.Common.TFM.Types
import Hexlude

readMathSymbolParams :: Ser.Get MathSymbolParams
readMathSymbolParams =
  MathSymbolParams
    <$> (LengthDesignSize <$> TFM.Get.Common.getFixWord)
    <*> (LengthDesignSize <$> TFM.Get.Common.getFixWord)
    <*> (LengthDesignSize <$> TFM.Get.Common.getFixWord)
    <*> (LengthDesignSize <$> TFM.Get.Common.getFixWord)
    <*> (LengthDesignSize <$> TFM.Get.Common.getFixWord)
    <*> (LengthDesignSize <$> TFM.Get.Common.getFixWord)
    <*> (LengthDesignSize <$> TFM.Get.Common.getFixWord)
    <*> (LengthDesignSize <$> TFM.Get.Common.getFixWord)
    <*> (LengthDesignSize <$> TFM.Get.Common.getFixWord)
    <*> (LengthDesignSize <$> TFM.Get.Common.getFixWord)
    <*> (LengthDesignSize <$> TFM.Get.Common.getFixWord)
    <*> (LengthDesignSize <$> TFM.Get.Common.getFixWord)
    <*> (LengthDesignSize <$> TFM.Get.Common.getFixWord)
    <*> (LengthDesignSize <$> TFM.Get.Common.getFixWord)
    <*> (LengthDesignSize <$> TFM.Get.Common.getFixWord)

readMathExtensionParams :: Ser.Get MathExtensionParams
readMathExtensionParams =
  MathExtensionParams
    <$> (LengthDesignSize <$> TFM.Get.Common.getFixWord)
    <*> (LengthDesignSize <$> TFM.Get.Common.getFixWord)
    <*> (LengthDesignSize <$> TFM.Get.Common.getFixWord)
    <*> (LengthDesignSize <$> TFM.Get.Common.getFixWord)
    <*> (LengthDesignSize <$> TFM.Get.Common.getFixWord)
    <*> (LengthDesignSize <$> TFM.Get.Common.getFixWord)

getFontParams :: Maybe [ASCII.Char] -> Ser.Get FontParams
getFontParams scheme =
  FontParams
    <$> TFM.Get.Common.getFixWord
    <*> (LengthDesignSize <$> TFM.Get.Common.getFixWord)
    <*> (LengthDesignSize <$> TFM.Get.Common.getFixWord)
    <*> (LengthDesignSize <$> TFM.Get.Common.getFixWord)
    <*> (LengthDesignSize <$> TFM.Get.Common.getFixWord)
    <*> (LengthDesignSize <$> TFM.Get.Common.getFixWord)
    <*> (LengthDesignSize <$> TFM.Get.Common.getFixWord)
    <*> case ASCII.charListToText <$> scheme of
      Just "TeX math symbols" ->
        Just . MathSymbolFontParams <$> readMathSymbolParams
      Just "TeX math extension" ->
        Just . MathExtensionFontParams <$> readMathExtensionParams
      _ ->
        pure Nothing
