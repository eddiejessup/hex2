module Hex.Common.TFM.Get.CharInfo where

import Effectful.Serialize.Get qualified as Get
import Hex.Common.TFM.Get.Types qualified as Internal
import Hexlude

data CharInfo = CharInfo
  { widthIdx :: Word8,
    heightIdx :: Word8,
    depthIdx :: Word8,
    italicCorrectionIdx :: Word8,
    tag :: Internal.Tag,
    charRemainder :: Word8
  }
  deriving stock (Show, Generic)

getCharInfo :: (Get.Get :> es) => Eff es CharInfo
getCharInfo =
  do
    widthIdx <- Get.getWord8
    heightDepthByte <- Get.getWord8
    italicTagByte <- Get.getWord8
    charRemainder <- Get.getWord8
    let tag = case italicTagByte .&. 0x3 of
          0 -> Internal.Plain
          1 -> Internal.LigKern
          2 -> Internal.Chain
          3 -> Internal.Extensible
          _ -> panic "impossible"
    pure
      CharInfo
        { widthIdx,
          heightIdx = heightDepthByte `shiftR` 4,
          depthIdx = heightDepthByte .&. 0xF,
          italicCorrectionIdx = italicTagByte `shiftR` 6,
          tag,
          charRemainder
        }
