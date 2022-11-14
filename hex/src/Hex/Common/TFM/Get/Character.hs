module Hex.Common.TFM.Get.Character where

import Data.IntMap qualified as IntMap
import GHC.Num
import Hex.Common.Box qualified as Box
import Hex.Common.TFM.Get.CharInfo qualified as TFM.Get.CharInfo
import Hex.Common.TFM.Get.Types qualified as Internal
import Hex.Common.TFM.Types
import Hexlude

character ::
  forall es.
  Error TFMError :> es =>
  [Recipe] ->
  [LengthDesignSize] -> -- Width
  [LengthDesignSize] -> -- Height
  [LengthDesignSize] -> -- Depth
  [LengthDesignSize] -> -- ItalicCorrection
  TFM.Get.CharInfo.CharInfo ->
  Eff es Character
character recipes widths heights depths italicCorrs charInfo =
  do
    width <- note IndexError $ dimAtEith widths $ TFM.Get.CharInfo.widthIdx charInfo
    height <- note IndexError $ dimAtEith heights $ TFM.Get.CharInfo.heightIdx charInfo
    depth <- note IndexError $ dimAtEith depths $ TFM.Get.CharInfo.depthIdx charInfo
    let dims = Box.BoxDims {boxWidth = width, boxHeight = height, boxDepth = depth}
    italicCorrection <- note IndexError $ dimAtEith italicCorrs $ TFM.Get.CharInfo.italicCorrectionIdx charInfo
    let remainder = TFM.Get.CharInfo.charRemainder charInfo
    -- If the character is special, get its particular extra attributes.
    special <- case TFM.Get.CharInfo.tag charInfo of
      Internal.Plain -> pure Nothing
      Internal.LigKern -> pure $ Just $ LigKernIndex remainder
      Internal.Chain -> pure $ Just $ NextLargerChar remainder
      Internal.Extensible -> do
        recipe <- note IndexError $ atMay recipes (fromIntegral @Word8 @Int remainder)
        pure $ Just $ ExtensibleRecipeSpecial recipe
    pure
      Character
        { dims,
          italicCorrection,
          special
        }
  where
    -- Get a dimension from some dimension table, at some index
    -- The relation `width[0] = height[0] = depth[0] = italic[0] = 0` should
    -- hold, so that an index of zero implies a value of zero. A character is
    -- valid if and only if it lies between `bc` and `ec` and has a nonzero
    -- `width_index`.
    dimAtEith :: [LengthDesignSize] -> Word8 -> Maybe LengthDesignSize
    dimAtEith xs i
      | i == 0 = Just zeroLengthDesignSize
      | otherwise = atMay xs (fromIntegral @Word8 @Int i)

characters ::
  Error TFMError :> es =>
  Word16 ->
  [TFM.Get.CharInfo.CharInfo] ->
  [Recipe] ->
  [LengthDesignSize] -> -- Width
  [LengthDesignSize] -> -- Height
  [LengthDesignSize] -> -- Depth
  [LengthDesignSize] -> -- ItalicCorrection
  Eff es (IntMap Character)
characters minCode charInfos recipes widths heights depths italicCorrs =
  do
    charList <- mapM (character recipes widths heights depths italicCorrs) charInfos
    pure $ IntMap.fromList $ (\(idx, c) -> (idx + fromIntegral @Word16 @Int minCode, c)) <$> indexed charList
