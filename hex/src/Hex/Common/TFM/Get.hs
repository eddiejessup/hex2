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
    fontParams :: FontParams,
    smallestCharCode :: Word16,
    largestCharCode :: Word16
  }
  deriving stock (Show, Generic)

parseTFMBytesRaw :: (Log.HexLog :> es, Error TFMError :> es, Get.Get :> es) => Eff es RawFont
parseTFMBytesRaw = do
  tableParams <- TFM.Get.TableParams.getTableParams
  Log.debugLog "Parsed table parameters"
  header <- TFM.Get.Common.getFixedLength (tableParams.headerLength) TFM.Get.Header.getHeader
  Log.debugLog "Parsed header"
  charInfos <- getChunkedTable (tableParams.characterInfosLength) $ (TFM.Get.CharInfo.getCharInfo)
  Log.debugLog "Parsed character info"
  widths <- getLengthsTable (tableParams.widthsLength)
  Log.debugLog "Parsed widths"
  heights <- getLengthsTable (tableParams.heightsLength)
  Log.debugLog "Parsed heights"
  depths <- getLengthsTable (tableParams.depthsLength)
  Log.debugLog "Parsed depths"
  italicCorrections <- getLengthsTable (tableParams.italicCorrectionsLength)
  Log.debugLog "Parsed italic corrections"
  ligKernCommands <- getChunkedTable (tableParams.ligKernCommandsLength) $ (TFM.Get.LigKernCommand.getLigKernCommand)
  Log.debugLog "Parsed ligature/kerning commands"
  kernOps <- getLengthsTable (tableParams.kernOpsLength)
  Log.debugLog "Parsed kerning operations"
  recipes <- getChunkedTable (tableParams.extensibleRecipesLength) $ (TFM.Get.Recipe.getExtensibleRecipe)
  Log.debugLog "Parsed extensible recipes"
  let scheme = TFM.Get.Header.characterCodingScheme header
  fontParams <- TFM.Get.FontParams.getFontParams scheme
  Log.debugLog "Parsed font parameters"

  pure RawFont {header, charInfos, widths, heights, depths, italicCorrections, ligKernCommands, kernOps, recipes, fontParams, smallestCharCode = tableParams.smallestCharCode, largestCharCode = tableParams.largestCharCode}
  where
    getLengthsTable tableLength =
      getChunkedTable tableLength TFM.Get.Common.getDesignSizeLength

    getChunkedTable tableLength getChunk =
      TFM.Get.Common.getFixedLength tableLength (TFM.Get.Common.getChunks getChunk)

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
        characterCodingScheme = ASCII.charListToText <$> rawFont.header.characterCodingScheme,
        fontFamily = ASCII.charListToText <$> rawFont.header.fontFamily,
        params = rawFont.fontParams,
        ligKerns = ligKernInstrs,
        characters
      }
