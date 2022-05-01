module Hex.Common.TFM.Get.CharInfo where

import Data.Serialize.Get qualified as Ser
import Hex.Common.TFM.Types
import Hexlude

data CharInfo = CharInfo
  { widthIdx :: Word8,
    heightIdx :: Word8,
    depthIdx :: Word8,
    italicCorrectionIdx :: Word8,
    tag :: Tag,
    charRemainder :: Word8
  }

getCharInfo :: Ser.Get CharInfo
getCharInfo =
  do
    widthIdx <- Ser.getWord8
    heightDepthByte <- Ser.getWord8
    italicTagByte <- Ser.getWord8
    charRemainder <- Ser.getWord8
    let tag = case italicTagByte .&. 0x3 of
          0 -> Plain
          1 -> LigKern
          2 -> Chain
          3 -> Extensible
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
