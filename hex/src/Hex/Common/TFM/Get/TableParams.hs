module Hex.Common.TFM.Get.TableParams where

import Effectful.Serialize.Get qualified as Get
import Formatting qualified as F
import GHC.Num
import Hex.Capability.Log.Interface qualified as Log
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

-- The minimum length of the header, in bytes.
headerDataLengthMin :: Int
headerDataLengthMin = 72

-- The position where the header starts.
headerPointer :: Int
headerPointer = 24

getTableParams :: (Log.HexLog :> es, Get.Get :> es) => Eff es TableParams
getTableParams = do
  -- Read and set table lengths.
  fileLength <- getLength
  Log.debugLog $ F.sformat ("Read file table length: " |%| F.int) fileLength
  Log.debugLog "Parsing 'header' table length"
  headerLength <- getLength
  Log.debugLog $ "Parsed 'header' table length: " <> show headerLength
  when (headerLength < headerDataLengthMin) $
    Log.warnLog $
      "Header length, "
        <> show headerLength
        <> "is less than the minimum specification length, "
        <> show headerDataLengthMin

  Log.debugLog "Parsing smallest char-code"
  smallestCharCode <- Get.getWord16be
  Log.debugLog $ "Parsed smallest char-code: " <> show smallestCharCode
  Log.debugLog "Parsing largest char-code"
  largestCharCode <- Get.getWord16be
  Log.debugLog $ "Parsed largest char-code: " <> show largestCharCode
  let characterInfosLength = asLength $ largestCharCode - smallestCharCode + 1

  -- Read the lengths of all tables after and including the 'Width' table.
  Log.debugLog "Parsing 'widths' table length"
  widthsLength <- getLength
  Log.debugLog $ "Parsed 'widths' table length: " <> show widthsLength
  Log.debugLog "Parsing 'heights' table length"
  heightsLength <- getLength
  Log.debugLog $ "Parsed 'heights' table length: " <> show heightsLength
  Log.debugLog "Parsing 'depths' table length"
  depthsLength <- getLength
  Log.debugLog $ "Parsed 'depths' table length: " <> show depthsLength
  Log.debugLog "Parsing 'italic-corrections' table length"
  italicCorrectionsLength <- getLength
  Log.debugLog $ "Parsed 'italic-corrections' table length: " <> show italicCorrectionsLength
  Log.debugLog "Parsing 'lig-kern commands' table length"
  ligKernCommandsLength <- getLength
  Log.debugLog $ "Parsed 'lig-kern commands' table length: " <> show ligKernCommandsLength
  Log.debugLog "Parsing 'kern-ops' table length"
  kernOpsLength <- getLength
  Log.debugLog $ "Parsed 'kern-ops' table length: " <> show kernOpsLength
  Log.debugLog "Parsing 'extensible-recipes' table length"
  extensibleRecipesLength <- getLength
  Log.debugLog $ "Parsed 'extensible-recipes' table length: " <> show extensibleRecipesLength
  Log.debugLog "Parsing 'font parameters' table length"
  fontParametersLength <- getLength
  Log.debugLog $ "Parsed 'font parameters' table length: " <> show fontParametersLength

  -- lf=24+lh+4*(ec-bc+1)+nw+nh+nd+ni+nl+nk+ne+np)
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

  when (fileLength /= inferredFileLength) $ do
    Log.errLog $ "Sum of table lengths, " <> show inferredFileLength <> ", does not match stated file length " <> show fileLength

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
