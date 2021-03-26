module Hex.App where

import Formatting qualified as F
import Hex.HexState.Instances.MonadHexState ()
import Hex.HexState.Instances.MonadHexState qualified as H.St
import Hex.HexState.Type qualified as H.St
import Hex.Interpret.Build.List.Elem qualified as H.Inter.B.List
import Hex.Interpret.Build.List.Horizontal.Paragraph.Break qualified as Hex.Inter.B.List.Para
import Hex.Interpret.CommandHandler.AllMode qualified as H.Inter.Comm.AllMode
import Hex.Interpret.CommandHandler.MainVMode qualified as H.Inter.Comm.MainV
import Hex.Interpret.CommandHandler.ParaMode qualified as H.Inter.Comm.Para
import Hex.Interpret.Evaluate.Impl qualified as H.Inter.Eval
import Hex.Lex.Types qualified as H.Lex
import Hex.MonadHexState.Interface
import Hex.Parse.CharSource qualified as H.Par.ChrSrc
import Hex.Parse.MonadParse.Impls.MonadTokenSource qualified as H.Par.Par
import Hex.Parse.MonadTokenSource.Impls.CharSource ()
import Hex.Symbol.Tokens
import Hex.TFM.Get qualified as H.TFM
import Hexlude

data AppState = AppState H.St.HexState H.Par.ChrSrc.CharSource
  deriving stock (Generic)

newAppState :: ByteString -> AppState
newAppState chrs = AppState H.St.newHexState (H.Par.ChrSrc.newCharSource chrs)

data AppError
  = AppLexError H.Lex.LexError
  | AppParseError H.Par.Par.ParseError
  | AppInterpretError H.Inter.Comm.AllMode.InterpretError
  | AppEvaluationError H.Inter.Eval.EvaluationError
  | AppHexStateError H.St.HexStateError
  | AppTFMError H.TFM.TFMError
  deriving stock (Generic, Show)

runApp ::
  AppState ->
  StateT AppState (ExceptT AppError IO) a ->
  IO (Either AppError (a, AppState))
runApp chrSrc st = runExceptT (runStateT st chrSrc)

runNewApp ::
  ByteString ->
  StateT AppState (ExceptT AppError IO) a ->
  IO (Either AppError (a, AppState))
runNewApp = runApp . newAppState

-- H.Inter.Comm.Para.charAsBox (H.Codes.CharCode_ 'a') >>= print

-- > evalNewApp "" (getCategory currentFontSpacing)
-- Right Escape
-- > evalNewApp "" (getCategory (CharCode_ '\\'))
-- Right Escape
-- > evalNewApp "" (getIntParameter PreTolerance)
-- Right (HexInt {unInt = 0})
-- > evalNewApp "\\relax" (getLexToken)
-- Right (Just (ControlSequenceLexToken "relax"))
-- > evalNewApp "\\relax" (getResolvedToken Resolving)
-- Right (Just (ControlSequenceLexToken "relax",PrimitiveToken RelaxTok))
-- > evalNewApp "a" (parseCommand)
-- Right (HModeCommand (AddCharacter (CharRef 97)))
-- > evalNewApp "\\relax" (buildMainVList)
-- Left (AppParseError UnexpectedEndOfInput)
-- > evalNewApp "\\relax\\end" (buildMainVList)
-- Right (VList (fromList []))
evalNewApp :: ByteString -> StateT AppState (ExceptT AppError IO) a -> IO (Either AppError a)
evalNewApp chrs = fmap (fmap fst) <$> runNewApp chrs

unsafeEvalNewApp :: ByteString -> StateT AppState (ExceptT AppError IO) a -> IO a
unsafeEvalNewApp chrs app = do
  evalNewApp chrs app >>= \case
    Left e -> panic $ "got error: " <> show e
    Right v -> pure v

testAppLoadSelectFont :: StateT AppState (ExceptT AppError IO) ()
testAppLoadSelectFont = do
  (fNr, _name) <- loadFont "cmr10.tfm"
  selectFont fNr Local

testApp :: StateT AppState (ExceptT AppError IO) ()
testApp = do
  (fNr, name) <- loadFont "cmr10.tfm"
  print name
  currentFontSpaceGlue >>= print
  selectFont fNr Local
  currentFontSpaceGlue >>= print

  vList <- H.Inter.Comm.MainV.buildMainVList

  putText $ F.sformat H.Inter.B.List.fmtVList vList

testParaAppHLayList :: StateT AppState (ExceptT AppError IO) H.Inter.B.List.HList
testParaAppHLayList = do
  testAppLoadSelectFont
  (_endParaReason, hList) <- H.Inter.Comm.Para.buildParaList DoNotIndent
  pure $ Hex.Inter.B.List.Para.finaliseHList hList

testParaApp :: StateT AppState (ExceptT AppError IO) ()
testParaApp = do
  testAppLoadSelectFont

  (endParaReason, hList) <- H.Inter.Comm.Para.buildParaList DoNotIndent
  print endParaReason

  putText "{{{{{{{{{{{{{{{{{{{{{{"
  putText "Raw HList:"
  putText "==================="
  putText $ F.sformat H.Inter.B.List.fmtHListMultiLine hList
  putText "}}}}}}}}}}}}}}}}}}}}}}"
  putText ""

  let layHList = Hex.Inter.B.List.Para.finaliseHList hList
  putText "{{{{{{{{{{{{{{{{{{{{{{"
  putText "Finalised HList:"
  putText "==================="
  putText $ F.sformat H.Inter.B.List.fmtHListMultiLine layHList
  putText "}}}}}}}}}}}}}}}}}}}}}}"
  putText ""

  -- let chunks = Hex.Inter.B.List.Para.chunkHList layHList
  -- HACK:
  let chunks = Hex.Inter.B.List.Para.chunkHList hList
  putText ""
  putText "{{{{{{{{{{{{{{{{{{{{{{"
  putText "HList elems in chunks:"
  putText "==================="
  putText $ F.sformat (F.intercalated "\n\n" F.shown) chunks
  putText "}}}}}}}}}}}}}}}}}}}}}}"
  putText ""

  -- let breakSequences = Hex.Inter.B.List.Para.allBreakSequences chunks
  -- putText "{{{{{{{{{{{{{{{{{{{{{{"
  -- putText "All paras:"
  -- putText "==================="

  -- ifor_ breakSequences $ \i breakSequence -> do
  --   putText ""
  --   putText "[[[[[[[[[[[[[[[[[[[[["
  --   putText $ "Para sequence " <> show i <> " (" <> show (length breakSequence) <> " lines):"
  --   putText "~~~~~~~~~~~~~~~~~~~"
  --   putText $ renderTextContainer $ breakSequence <&> \line -> "Line:\n" <> Hex.Inter.B.List.Para.renderLine line
  --   putText "]]]]]]]]]]]]]]]]]]]]]"
  --   putText ""
  -- putText "}}}}}}}}}}}}}}}}}}}}}}"

  pure ()
