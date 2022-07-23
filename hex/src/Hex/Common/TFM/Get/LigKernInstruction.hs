module Hex.Common.TFM.Get.LigKernInstruction where

import Hex.Common.TFM.Get.LigKernCommand (LigKernCommand (..))
import Hex.Common.TFM.Types
import Hexlude

minOpByteForKern :: Word8
minOpByteForKern = 128

ligKernInstruction :: Error TFMError :> es => [KernOp] -> LigKernCommand -> Eff es LigKernInstr
ligKernInstruction kernOps LigKernCommand {skipByte, commandNextChar = nextChar, opByte, commandRemainder = remainder} = do
  operation <-
    if opByte >= minOpByteForKern
      then do
        let kernOpIdx = 256 * (fromIntegral opByte - fromIntegral minOpByteForKern) + fromIntegral remainder
        kernOp <- note (TFMError $ "No kern at index " <> show kernOpIdx) $ atMay kernOps kernOpIdx
        pure $ LigKernKernOp kernOp
      else
        pure $
          LigKernLigOp
            LigatureOp
              { ligatureChar = remainder,
                charsToPassOver = opByte `shift` 2,
                deleteCurrentChar = (opByte .&. 0x02) == 0,
                deleteNextChar = (opByte .&. 0x01) == 0
              }
  pure
    LigKernInstr
      { stop = skipByte >= 128,
        nextChar,
        operation
      }

-- TODO: Implement
-- when (firstSkipByte == 255) $ Left "Unsupported: Right boundary characters"
-- when (firstSkipByte > 128) $ Left "Unsupported: Large LigKern arrays"
-- when (lastSkipByte == 255) $ Left "Unsupported: Left boundary characters"
-- where
-- edgesMay xs = do
--     hd <- headMay xs
--     lst <- lastMay xs
--     pure (hd, lst)
