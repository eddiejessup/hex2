module Hex.Common.TFM.Get.Character where

import Data.IntMap qualified as IntMap
import Hex.Common.TFM.Get.CharInfo qualified as TFM.Get.CharInfo
import Hex.Common.TFM.Types
import Hexlude

character ::
  forall m.
  MonadError Text m =>
  [Recipe] ->
  [LengthDesignSize] -> -- Width
  [LengthDesignSize] -> -- Height
  [LengthDesignSize] -> -- Depth
  [LengthDesignSize] -> -- ItalicCorrection
  TFM.Get.CharInfo.CharInfo ->
  m Character
character recipes widths heights depths italicCorrs charInfo =
  do
    width <- note "Bad width index" $ dimAtEith widths $ TFM.Get.CharInfo.widthIdx charInfo
    height <- note "Bad height index" $ dimAtEith heights $ TFM.Get.CharInfo.heightIdx charInfo
    depth <- note "Bad depth index" $ dimAtEith depths $ TFM.Get.CharInfo.depthIdx charInfo
    italicCorrection <- note "Bad italic correction index" $ dimAtEith italicCorrs $ TFM.Get.CharInfo.italicCorrectionIdx charInfo
    let remainder = TFM.Get.CharInfo.charRemainder charInfo
    -- If the character is special, get its particular extra attributes.
    special <- case TFM.Get.CharInfo.tag charInfo of
      Plain -> pure Nothing
      LigKern -> pure $ Just $ LigKernIndex remainder
      Chain -> pure $ Just $ NextLargerChar remainder
      Extensible -> do
        recipe <- note "Bad recipe index" $ atMay recipes (fromIntegral @Word8 @Int remainder)
        pure $ Just $ ExtensibleRecipeSpecial recipe
    pure
      Character
        { width,
          height,
          depth,
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
  MonadError Text m =>
  Word16 ->
  [TFM.Get.CharInfo.CharInfo] ->
  [Recipe] ->
  [LengthDesignSize] -> -- Width
  [LengthDesignSize] -> -- Height
  [LengthDesignSize] -> -- Depth
  [LengthDesignSize] -> -- ItalicCorrection
  m (IntMap Character)
characters minCode charInfos recipes widths heights depths italicCorrs =
  do
    charList <- mapM (character recipes widths heights depths italicCorrs) charInfos
    pure $ IntMap.fromList $ (\(idx, c) -> (idx + fromIntegral @Word16 @Int minCode, c)) <$> indexed charList
  where
    -- Taken from package ilist-0.4.0.1.
    indexed :: [a] -> [(Int, a)]
    indexed xs = go 0 xs
      where
        go i (a : as) = (i, a) : go (i + 1) as
        go _ _ = []
