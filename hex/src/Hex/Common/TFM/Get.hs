module Hex.Common.TFM.Get where

import ASCII qualified
import Data.Serialize qualified as Ser
import Data.Text qualified as Tx
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

parseTFMBytes :: forall es. Error TFMError :> es => ByteString -> Eff es Font
parseTFMBytes bs = do
  tableParams <- runGetText TFM.Get.TableParams.getTableParams bs

  header <- runGetText TFM.Get.Header.getHeader $ TFM.Get.TableParams.headerBytes tableParams
  charInfos <- runGetText (TFM.Get.Common.getChunks TFM.Get.CharInfo.getCharInfo) $ TFM.Get.TableParams.characterInfoBytes tableParams
  widths <- runGetText (TFM.Get.Common.getChunks (LengthDesignSize <$> TFM.Get.Common.getFixWord)) $ TFM.Get.TableParams.widthBytes tableParams
  heights <- runGetText (TFM.Get.Common.getChunks (LengthDesignSize <$> TFM.Get.Common.getFixWord)) $ TFM.Get.TableParams.heightBytes tableParams
  depths <- runGetText (TFM.Get.Common.getChunks (LengthDesignSize <$> TFM.Get.Common.getFixWord)) $ TFM.Get.TableParams.depthBytes tableParams
  italicCorrs <- runGetText (TFM.Get.Common.getChunks (LengthDesignSize <$> TFM.Get.Common.getFixWord)) $ TFM.Get.TableParams.italicCorrectionBytes tableParams
  ligKernCommands <- runGetText (TFM.Get.Common.getChunks TFM.Get.LigKernCommand.getLigKernCommand) $ TFM.Get.TableParams.ligKernBytes tableParams
  kernOps <- runGetText (TFM.Get.Common.getChunks (KernOp . LengthDesignSize <$> TFM.Get.Common.getFixWord)) $ TFM.Get.TableParams.kernBytes tableParams
  recipes <- runGetText (TFM.Get.Common.getChunks TFM.Get.Recipe.getExtensibleRecipe) $ TFM.Get.TableParams.extensibleRecipeBytes tableParams
  let scheme = TFM.Get.Header.characterCodingScheme header
  params <- runGetText (TFM.Get.FontParams.getFontParams scheme) $ TFM.Get.TableParams.fontParameterBytes tableParams
  ligKernInstrs <- mapM (TFM.Get.LigKernInstruction.ligKernInstruction kernOps) ligKernCommands
  chars <- TFM.Get.Character.characters (TFM.Get.TableParams.smallestCharCode tableParams) charInfos recipes widths heights depths italicCorrs

  pure
    Font
      { checksum = TFM.Get.Header.checksum header,
        designFontSize = TFM.Get.Header.designFontSize header,
        characterCodingScheme = ASCII.charListToText <$> TFM.Get.Header.characterCodingScheme header,
        family = ASCII.charListToText <$> TFM.Get.Header.family header,
        params,
        ligKerns = ligKernInstrs,
        characters = chars
      }
  where
    runGetText :: Ser.Get d -> ByteString -> Eff es d
    runGetText m b = case Ser.runGet m b of
      Left e -> throwError (TFMError $ Tx.pack e)
      Right v -> pure v
