module Hex.Common.TFM.Get.FontParams where

import Effectful.Serialize.Get qualified as Get
import Hex.Capability.Log.Interface qualified as Log
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

getFontParams :: (Log.HexLog :> es, Get.Get :> es) => Maybe CharacterCodingScheme -> Eff es Bool -> Eff es FontParams
getFontParams scheme _atEndOfTable = do
  Log.debugLog "Parsing font parameter 'slant'"
  slant <- TFM.Get.Common.getFixWord
  Log.debugLog $ "Parsed font parameter 'slant' with value: " <> show slant
  Log.debugLog "Parsing font parameter 'spacing'"
  spacing <- getDesignSizeLength
  Log.debugLog $ "Parsed font parameter 'spacing' with value: " <> show spacing
  Log.debugLog "Parsing font parameter 'spaceStretch'"
  spaceStretch <- getDesignSizeLength
  Log.debugLog $ "Parsed font parameter 'spaceStretch' with value: " <> show spaceStretch
  Log.debugLog "Parsing font parameter 'spaceShrink'"
  spaceShrink <- getDesignSizeLength
  Log.debugLog $ "Parsed font parameter 'spaceShrink' with value: " <> show spaceShrink
  Log.debugLog "Parsing font parameter 'xHeight'"
  xHeight <- getDesignSizeLength
  Log.debugLog $ "Parsed font parameter 'xHeight' with value: " <> show xHeight
  Log.debugLog "Parsing font parameter 'quad'"
  quad <- getDesignSizeLength
  Log.debugLog $ "Parsed font parameter 'quad' with value: " <> show quad
  extraSpace <- case scheme of
    Just MathItalicScheme ->
      pure Nothing
    _ ->
      Just <$> getDesignSizeLength
  extraParams <- case scheme of
    Just MathSymbolsScheme ->
      Just <$> MathSymbolFontParams <$> readMathSymbolParams
    Just MathExtensionScheme ->
      Just <$> MathExtensionFontParams <$> readMathExtensionParams
    _ ->
      pure Nothing

  pure
    FontParams
      { slant,
        spacing,
        spaceStretch,
        spaceShrink,
        xHeight,
        quad,
        extraSpace,
        extraParams
      }
