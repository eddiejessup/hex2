module Hex.Common.TFM.Get where

import Control.Arrow (left)
import Data.ByteString qualified as BS
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
import qualified Formatting as F

newtype TFMError = TFMError Text
  deriving stock (Generic, Show)

fmtTfmError :: Fmt TFMError a
fmtTfmError = "TFM Error: " |%| F.shown

parseTFMFile :: (MonadIO m, MonadError e m, AsType TFMError e) => FilePath -> m Font
parseTFMFile path =
  liftIO (BS.readFile path) <&> parseTFMBytes >>= \case
    Left err -> throwError $ injectTyped $ TFMError err
    Right v -> pure v

parseTFMFileIO :: FilePath -> IO (Either (Identity TFMError) Font)
parseTFMFileIO path = runExceptT (parseTFMFile path)

parseTFMBytes :: ByteString -> Either Text Font
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
        characterCodingScheme = TFM.Get.Header.characterCodingScheme header,
        family = TFM.Get.Header.family header,
        params,
        ligKerns = ligKernInstrs,
        characters = chars
      }
  where
    runGetText m b = left Tx.pack $ Ser.runGet m b
