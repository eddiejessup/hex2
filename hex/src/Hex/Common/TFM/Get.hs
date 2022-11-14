module Hex.Common.TFM.Get where

import ASCII qualified
import Effectful.Serialize.Get qualified as Get
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.TFM.Get.CharInfo qualified as TFM.Get.CharInfo
import Hex.Common.TFM.Get.Character qualified as TFM.Get.Character
import Hex.Common.TFM.Get.Common qualified as TFM.Get.Common
import Hex.Common.TFM.Get.FontParams qualified as TFM.Get.FontParams
import Hex.Common.TFM.Get.Header qualified as TFM.Get.Header
import Hex.Common.TFM.Get.LigKernCommand qualified as TFM.Get.LigKernCommand
import Hex.Common.TFM.Get.LigKernInstruction qualified as TFM.Get.LigKernInstruction
import Hex.Common.TFM.Get.Recipe qualified as TFM.Get.Recipe
import Hex.Common.TFM.Get.TableParams qualified as TFM.Get.TableParams
import Hex.Common.TFM.Types
import Hexlude

data RawFont = RawFont
  { header :: TFM.Get.Header.Header,
    charInfos :: [TFM.Get.CharInfo.CharInfo],
    widths :: [LengthDesignSize],
    heights :: [LengthDesignSize],
    depths :: [LengthDesignSize],
    italicCorrections :: [LengthDesignSize],
    ligKernCommands :: [TFM.Get.LigKernCommand.LigKernCommand],
    kernOps :: [LengthDesignSize],
    recipes :: [Recipe],
    fontParameters :: FontParams,
    smallestCharCode :: Word16,
    largestCharCode :: Word16
  }
  deriving stock (Show, Generic)

parseTFMBytesRaw :: (Log.HexLog :> es, Error TFMError :> es, Get.Get :> es) => Eff es RawFont
parseTFMBytesRaw = do
  Log.debugLog "Parsing table parameters"
  tableParams <- TFM.Get.TableParams.getTableParams
  Log.debugLog "Parsing header"
  header <- getTable tableParams.headerLength TFM.Get.Header.getHeader
  Log.debugLog "Parsing character info"
  charInfos <- getChunkedTable tableParams.characterInfosLength TFM.Get.CharInfo.getCharInfo
  Log.debugLog "Parsing widths"
  widths <- getLengthsTable tableParams.widthsLength
  Log.debugLog "Parsing heights"
  heights <- getLengthsTable tableParams.heightsLength
  Log.debugLog "Parsing depths"
  depths <- getLengthsTable tableParams.depthsLength
  Log.debugLog "Parsing italic corrections"
  italicCorrections <- getLengthsTable tableParams.italicCorrectionsLength
  Log.debugLog "Parsing ligature/kerning commands"
  ligKernCommands <- getChunkedTable tableParams.ligKernCommandsLength TFM.Get.LigKernCommand.getLigKernCommand
  Log.debugLog "Parsing kerning operations"
  kernOps <- getLengthsTable tableParams.kernOpsLength
  Log.debugLog "Parsing extensible recipes"
  recipes <- getChunkedTable tableParams.extensibleRecipesLength TFM.Get.Recipe.getExtensibleRecipe
  Log.debugLog "Parsing font parameters"
  fontParameters <- getTable tableParams.fontParametersLength $ TFM.Get.FontParams.getFontParams header.characterCodingScheme

  pure
    RawFont
      { header,
        charInfos,
        widths,
        heights,
        depths,
        italicCorrections,
        ligKernCommands,
        kernOps,
        recipes,
        fontParameters,
        smallestCharCode = tableParams.smallestCharCode,
        largestCharCode = tableParams.largestCharCode
      }
  where
    getLengthsTable tableLength =
      getChunkedTable tableLength TFM.Get.Common.getDesignSizeLength

    getChunkedTable tableLength getChunk =
      getTable tableLength (TFM.Get.Common.getChunks getChunk)

    getTable tableLength getBody = do
      (body, skipped) <- TFM.Get.Common.getFixedLength tableLength getBody
      when (skipped > 0) $ Log.warnLog $ "Skipped unused bytes: " <> show skipped
      pure body

parseTFMBytes :: (Log.HexLog :> es, Error TFMError :> es) => ByteString -> Eff es Font
parseTFMBytes bs = do
  rawFont <-
    runError @Text (Get.runGet bs parseTFMBytesRaw) >>= \case
      Left (stack, e) -> throwError $ ParseError $ e <> "; " <> show stack
      Right v -> pure v
  ligKernInstrs <- mapM (TFM.Get.LigKernInstruction.ligKernInstruction rawFont.kernOps) rawFont.ligKernCommands
  characters <- TFM.Get.Character.characters rawFont.smallestCharCode rawFont.charInfos rawFont.recipes rawFont.widths rawFont.heights rawFont.depths rawFont.italicCorrections

  pure
    Font
      { checksum = rawFont.header.checksum,
        designFontSize = rawFont.header.designFontSize,
        characterCodingScheme = rawFont.header.characterCodingScheme,
        fontFamily = ASCII.charListToText <$> rawFont.header.fontFamily,
        params = rawFont.fontParameters,
        ligKerns = ligKernInstrs,
        characters
      }
