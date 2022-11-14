module Hex.Common.TFM.Get.TableParams where

import Effectful.Serialize.Get qualified as Get
import Formatting qualified as F
import GHC.Num
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.TFM.Types
import Hexlude

data TableParams = TableParams
  { headerLength :: Int,
    characterInfosLength :: Int,
    widthsLength :: Int,
    heightsLength :: Int,
    depthsLength :: Int,
    italicCorrectionsLength :: Int,
    ligKernCommandsLength :: Int,
    kernOpsLength :: Int,
    extensibleRecipesLength :: Int,
    fontParametersLength :: Int,
    smallestCharCode :: Word16,
    largestCharCode :: Word16
  }

-- The minimum length of the header.
headerDataLengthWordsMin :: Int
headerDataLengthWordsMin = 18

-- The position where the header starts.
headerPointer :: Int
headerPointer = 24

getTableParams :: (Log.HexLog :> es, Get.Get :> es, Error TFMError :> es) => Eff es TableParams
getTableParams = do
  -- Read and set table lengths.
  Log.debugLog "getTableParams"
  fileLength <- getLength
  Log.debugLog $ F.sformat ("Read file length: " |%| F.int) fileLength
  headerLength <- max headerDataLengthWordsMin <$> getLength
  smallestCharCode <- Get.getWord16be
  largestCharCode <- Get.getWord16be
  let characterInfosLength = asLength $ largestCharCode - smallestCharCode + 1

  -- Read the lengths of all tables after and including the 'Width' table.
  widthsLength <- getLength
  heightsLength <- getLength
  depthsLength <- getLength
  italicCorrectionsLength <- getLength
  ligKernCommandsLength <- getLength
  kernOpsLength <- getLength
  extensibleRecipesLength <- getLength
  fontParametersLength <- getLength

  let inferredFileLength =
        headerPointer
          + headerLength
          + characterInfosLength
          + widthsLength
          + heightsLength
          + depthsLength
          + italicCorrectionsLength
          + ligKernCommandsLength
          + kernOpsLength
          + extensibleRecipesLength
          + fontParametersLength

  when (fileLength /= inferredFileLength) $
    throwError BadTableLengths

  pure
    TableParams
      { headerLength,
        characterInfosLength,
        widthsLength,
        heightsLength,
        depthsLength,
        italicCorrectionsLength,
        ligKernCommandsLength,
        kernOpsLength,
        extensibleRecipesLength,
        fontParametersLength,
        smallestCharCode,
        largestCharCode
      }
  where
    getLength = asLength <$> Get.getWord16be

    asLength = fromIntegral @Word16 @Int . Get.wordToByte
