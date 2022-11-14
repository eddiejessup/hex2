module Hex.Common.TFM.Get.LigKernCommand where

import Effectful.Serialize.Get qualified as Get
import Hexlude

data LigKernCommand = LigKernCommand
  { skipByte :: Word8,
    commandNextChar :: Word8,
    opByte :: Word8,
    commandRemainder :: Word8
  }
  deriving stock (Show)

getLigKernCommand :: (Get.Get :> es) => Eff es LigKernCommand
getLigKernCommand =
  LigKernCommand <$> Get.getWord8 <*> Get.getWord8 <*> Get.getWord8 <*> Get.getWord8
