module Hex.TFM.Get.FontParams where

import ASCII qualified
import Data.Serialize.Get qualified as Ser
import Hex.Quantity qualified as H.Q
import Hex.TFM.Get.Common qualified as H.TFM.Get.Common
import Hex.TFM.Types
import Hexlude

readMathSymbolParams :: Ser.Get MathSymbolParams
readMathSymbolParams =
  MathSymbolParams
    <$> (H.Q.LengthDesignSize <$> H.TFM.Get.Common.getFixWord)
    <*> (H.Q.LengthDesignSize <$> H.TFM.Get.Common.getFixWord)
    <*> (H.Q.LengthDesignSize <$> H.TFM.Get.Common.getFixWord)
    <*> (H.Q.LengthDesignSize <$> H.TFM.Get.Common.getFixWord)
    <*> (H.Q.LengthDesignSize <$> H.TFM.Get.Common.getFixWord)
    <*> (H.Q.LengthDesignSize <$> H.TFM.Get.Common.getFixWord)
    <*> (H.Q.LengthDesignSize <$> H.TFM.Get.Common.getFixWord)
    <*> (H.Q.LengthDesignSize <$> H.TFM.Get.Common.getFixWord)
    <*> (H.Q.LengthDesignSize <$> H.TFM.Get.Common.getFixWord)
    <*> (H.Q.LengthDesignSize <$> H.TFM.Get.Common.getFixWord)
    <*> (H.Q.LengthDesignSize <$> H.TFM.Get.Common.getFixWord)
    <*> (H.Q.LengthDesignSize <$> H.TFM.Get.Common.getFixWord)
    <*> (H.Q.LengthDesignSize <$> H.TFM.Get.Common.getFixWord)
    <*> (H.Q.LengthDesignSize <$> H.TFM.Get.Common.getFixWord)
    <*> (H.Q.LengthDesignSize <$> H.TFM.Get.Common.getFixWord)

readMathExtensionParams :: Ser.Get MathExtensionParams
readMathExtensionParams =
  MathExtensionParams
    <$> (H.Q.LengthDesignSize <$> H.TFM.Get.Common.getFixWord)
    <*> (H.Q.LengthDesignSize <$> H.TFM.Get.Common.getFixWord)
    <*> (H.Q.LengthDesignSize <$> H.TFM.Get.Common.getFixWord)
    <*> (H.Q.LengthDesignSize <$> H.TFM.Get.Common.getFixWord)
    <*> (H.Q.LengthDesignSize <$> H.TFM.Get.Common.getFixWord)
    <*> (H.Q.LengthDesignSize <$> H.TFM.Get.Common.getFixWord)

getFontParams :: Maybe [ASCII.Char] -> Ser.Get FontParams
getFontParams scheme =
  FontParams
    <$> H.TFM.Get.Common.getFixWord
    <*> (H.Q.LengthDesignSize <$> H.TFM.Get.Common.getFixWord)
    <*> (H.Q.LengthDesignSize <$> H.TFM.Get.Common.getFixWord)
    <*> (H.Q.LengthDesignSize <$> H.TFM.Get.Common.getFixWord)
    <*> (H.Q.LengthDesignSize <$> H.TFM.Get.Common.getFixWord)
    <*> (H.Q.LengthDesignSize <$> H.TFM.Get.Common.getFixWord)
    <*> (H.Q.LengthDesignSize <$> H.TFM.Get.Common.getFixWord)
    <*> case ASCII.charListToText <$> scheme of
      Just "TeX math symbols" ->
        Just . MathSymbolFontParams <$> readMathSymbolParams
      Just "TeX math extension" ->
        Just . MathExtensionFontParams <$> readMathExtensionParams
      _ ->
        pure Nothing
