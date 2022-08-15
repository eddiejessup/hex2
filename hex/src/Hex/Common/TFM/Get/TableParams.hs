module Hex.Common.TFM.Get.TableParams where

import Control.Monad.Fail (fail)
import Data.Serialize.Get qualified as Ser
import GHC.Num
import Hexlude

data TableParams = TableParams
  { headerBytes :: ByteString,
    characterInfoBytes :: ByteString,
    widthBytes :: ByteString,
    heightBytes :: ByteString,
    depthBytes :: ByteString,
    italicCorrectionBytes :: ByteString,
    ligKernBytes :: ByteString,
    kernBytes :: ByteString,
    extensibleRecipeBytes :: ByteString,
    fontParameterBytes :: ByteString,
    smallestCharCode :: Word16,
    largestCharCode :: Word16
  }

wordToByte :: (Num a) => a -> a
wordToByte = (* 4)

-- The minimum length of the header.
headerDataLengthWordsMin :: Word16
headerDataLengthWordsMin = 18

-- The position where the header starts.
headerPointerWords :: Word16
headerPointerWords = 6

headerPointerBytes :: Word16
headerPointerBytes = wordToByte headerPointerWords

getNWords :: Int -> Ser.Get ByteString
getNWords = Ser.getBytes . wordToByte

getTableParams :: Ser.Get TableParams
getTableParams = do
  -- Read and set table lengths.
  fileLengthWords <- Ser.getWord16be
  headerDataLengthWords <- max headerDataLengthWordsMin <$> Ser.getWord16be
  smallestCharCode <- Ser.getWord16be
  largestCharCode <- Ser.getWord16be

  -- Read the lengths of all tables after and including the 'Width' table.
  widthDataLengthWords <- Ser.getWord16be
  heightDataLengthWords <- Ser.getWord16be
  depthDataLengthWords <- Ser.getWord16be
  italicCorrectionDataLengthWords <- Ser.getWord16be
  ligKernDataLengthWords <- Ser.getWord16be
  kernDataLengthWords <- Ser.getWord16be
  extensibleRecipeDataLengthWords <- Ser.getWord16be
  fontParamDataLengthWords <- Ser.getWord16be

  let characterInfoDataLengthWords = largestCharCode - smallestCharCode + 1
      inferredFileLengthWords =
        headerPointerWords
          + headerDataLengthWords
          + characterInfoDataLengthWords
          + widthDataLengthWords
          + heightDataLengthWords
          + depthDataLengthWords
          + italicCorrectionDataLengthWords
          + ligKernDataLengthWords
          + kernDataLengthWords
          + extensibleRecipeDataLengthWords
          + fontParamDataLengthWords

  when (fileLengthWords /= inferredFileLengthWords) $
    fail $
      "Incorrect table lengths: read "
        <> show fileLengthWords
        <> " is not equal to inferred "
        <> show (headerDataLengthWords, (largestCharCode, smallestCharCode), widthDataLengthWords, heightDataLengthWords, depthDataLengthWords, italicCorrectionDataLengthWords, ligKernDataLengthWords, kernDataLengthWords, extensibleRecipeDataLengthWords, fontParamDataLengthWords)

  -- Read bytestring for each table.
  headerBytes <- getNWords $ fromIntegral @Word16 @Int headerDataLengthWords
  characterInfoBytes <- getNWords $ fromIntegral @Word16 @Int characterInfoDataLengthWords
  widthBytes <- getNWords $ fromIntegral @Word16 @Int widthDataLengthWords
  heightBytes <- getNWords $ fromIntegral @Word16 @Int heightDataLengthWords
  depthBytes <- getNWords $ fromIntegral @Word16 @Int depthDataLengthWords
  italicCorrectionBytes <- getNWords $ fromIntegral @Word16 @Int italicCorrectionDataLengthWords
  ligKernBytes <- getNWords $ fromIntegral @Word16 @Int ligKernDataLengthWords
  kernBytes <- getNWords $ fromIntegral @Word16 @Int kernDataLengthWords
  extensibleRecipeBytes <- getNWords $ fromIntegral @Word16 @Int extensibleRecipeDataLengthWords
  fontParameterBytes <- getNWords $ fromIntegral @Word16 @Int fontParamDataLengthWords

  pure
    TableParams
      { headerBytes,
        characterInfoBytes,
        widthBytes,
        heightBytes,
        depthBytes,
        italicCorrectionBytes,
        ligKernBytes,
        kernBytes,
        extensibleRecipeBytes,
        fontParameterBytes,
        smallestCharCode,
        largestCharCode
      }
