module Hex.Run.Interpret where

import Hex.Run.App (App, runLog)
import Hex.Stage.Build.ListElem (HList (..), VList (..))
import Hex.Stage.Build.ListExtractor.Interface (EndHListReason, IndentFlag (..), extractParagraphList)
import Hex.Stage.Build.ListExtractor.VList (extractMainVListImpl)
import Hex.Stage.Build.ListExtractor.HList (runListExtractor)
import Hex.Stage.Build.ListBuilder.Horizontal (runHListBuilder)
import Hexlude
import Hex.Stage.Interpret.AllMode (InterpretError)
import Hex.Common.HexInput.Impl (runHexInput)
import Hex.Common.HexInput.Interface (LexError)
import Hex.Common.HexEnv.Impl (runHexEnv)
import Hex.Common.HexState.Impl (runHexState, HexStateError)
import Hex.Common.TFM.Types (TFMError)

-- interpretInMainVMode :: App (Either TFMError (Either HexStateError (Either LexError (Either InterpretError VList))))
-- interpretInMainVMode =
--   runLog $
--   runHexEnv $
--   runErrorNoCallStack @TFMError $
--   runErrorNoCallStack @HexStateError $
--   runHexState $
--   runErrorNoCallStack @LexError $
--   runHexInput $
--   runErrorNoCallStack @InterpretError $
--   runHListBuilder $
--   runListExtractor $
--   extractMainVListImpl

-- interpretInParaMode :: Eff '[] (EndHListReason, HList)
-- interpretInParaMode = extractParagraphList Indent
