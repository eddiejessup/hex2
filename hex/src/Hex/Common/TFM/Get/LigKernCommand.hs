module Hex.Common.TFM.Get.LigKernCommand where

import Data.Serialize.Get qualified as Ser
import Hexlude

data LigKernCommand = LigKernCommand
  { skipByte :: Word8,
    commandNextChar :: Word8,
    opByte :: Word8,
    commandRemainder :: Word8
  }
  deriving stock (Show)

getLigKernCommand :: Ser.Get LigKernCommand
getLigKernCommand =
  LigKernCommand <$> Ser.getWord8 <*> Ser.getWord8 <*> Ser.getWord8 <*> Ser.getWord8
