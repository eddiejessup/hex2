module Hex.TFM.Get where

import Control.Arrow (left)
import Data.ByteString qualified as BS
import Data.Serialize qualified as Ser
import Data.Text qualified as Tx
import Hex.Quantity qualified as H.Q
import Hex.TFM.Get.CharInfo qualified as H.TFM.Get.CharInfo
import Hex.TFM.Get.Character qualified as H.TFM.Get.Character
import Hex.TFM.Get.Common qualified as H.TFM.Get.Common
import Hex.TFM.Get.FontParams qualified as H.TFM.Get.FontParams
import Hex.TFM.Get.Header qualified as H.TFM.Get.Header
import Hex.TFM.Get.LigKernCommand qualified as H.TFM.Get.LigKernCommand
import Hex.TFM.Get.LigKernInstruction qualified as H.TFM.Get.LigKernInstruction
import Hex.TFM.Get.Recipe qualified as H.TFM.Get.Recipe
import Hex.TFM.Get.TableParams qualified as H.TFM.Get.TableParams
import Hex.TFM.Types
import Hexlude

newtype TFMError = TFMError Text
  deriving stock (Generic, Show)

parseTFMFile :: (MonadIO m, MonadError e m, AsType TFMError e) => FilePath -> m Font
parseTFMFile path =
  liftIO (BS.readFile path) <&> parseTFMBytes >>= \case
    Left err -> throwError $ injectTyped $ TFMError err
    Right v -> pure v

parseTFMFileIO :: FilePath -> IO (Either (Identity TFMError) Font)
parseTFMFileIO path = runExceptT (parseTFMFile path)

parseTFMBytes :: ByteString -> Either Text Font
parseTFMBytes bs = do
  tableParams <- runGetText H.TFM.Get.TableParams.getTableParams bs

  header <- runGetText H.TFM.Get.Header.getHeader $ H.TFM.Get.TableParams.headerBytes tableParams
  charInfos <- runGetText (H.TFM.Get.Common.getChunks H.TFM.Get.CharInfo.getCharInfo) $ H.TFM.Get.TableParams.characterInfoBytes tableParams
  widths <- runGetText (H.TFM.Get.Common.getChunks (H.Q.LengthDesignSize <$> H.TFM.Get.Common.getFixWord)) $ H.TFM.Get.TableParams.widthBytes tableParams
  heights <- runGetText (H.TFM.Get.Common.getChunks (H.Q.LengthDesignSize <$> H.TFM.Get.Common.getFixWord)) $ H.TFM.Get.TableParams.heightBytes tableParams
  depths <- runGetText (H.TFM.Get.Common.getChunks (H.Q.LengthDesignSize <$> H.TFM.Get.Common.getFixWord)) $ H.TFM.Get.TableParams.depthBytes tableParams
  italicCorrs <- runGetText (H.TFM.Get.Common.getChunks (H.Q.LengthDesignSize <$> H.TFM.Get.Common.getFixWord)) $ H.TFM.Get.TableParams.italicCorrectionBytes tableParams
  ligKernCommands <- runGetText (H.TFM.Get.Common.getChunks H.TFM.Get.LigKernCommand.getLigKernCommand) $ H.TFM.Get.TableParams.ligKernBytes tableParams
  kernOps <- runGetText (H.TFM.Get.Common.getChunks (KernOp . H.Q.LengthDesignSize <$> H.TFM.Get.Common.getFixWord)) $ H.TFM.Get.TableParams.kernBytes tableParams
  recipes <- runGetText (H.TFM.Get.Common.getChunks H.TFM.Get.Recipe.getExtensibleRecipe) $ H.TFM.Get.TableParams.extensibleRecipeBytes tableParams
  let scheme = H.TFM.Get.Header.characterCodingScheme header
  params <- runGetText (H.TFM.Get.FontParams.getFontParams scheme) $ H.TFM.Get.TableParams.fontParameterBytes tableParams
  ligKernInstrs <- mapM (H.TFM.Get.LigKernInstruction.ligKernInstruction kernOps) ligKernCommands
  chars <- H.TFM.Get.Character.characters (H.TFM.Get.TableParams.smallestCharCode tableParams) charInfos recipes widths heights depths italicCorrs

  pure
    Font
      { checksum = H.TFM.Get.Header.checksum header,
        designFontSize = H.TFM.Get.Header.designFontSize header,
        characterCodingScheme = H.TFM.Get.Header.characterCodingScheme header,
        family = H.TFM.Get.Header.family header,
        params,
        ligKerns = ligKernInstrs,
        characters = chars
      }
  where
    runGetText m b = left Tx.pack $ Ser.runGet m b
