module Hex.App where

import Data.Text qualified as Tx
import Hex.Codes qualified as H.Codes
import Hex.HexState.Instances.MonadHexState ()
import Hex.HexState.Instances.MonadHexState qualified as H.St
import Hex.HexState.Type qualified as H.St
import Hex.Interpret.Build.List.Elem qualified as H.Inter.B.List
import Hex.Interpret.Build.List.Layout.Paragraph qualified as Hex.Inter.B.List.Para
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
  H.Inter.Comm.Para.charAsBox (H.Codes.CharCode_ 'a') >>= print
  H.Inter.Comm.Para.charAsBox (H.Codes.CharCode_ ' ') >>= print
  H.Inter.Comm.Para.charAsBox (H.Codes.CharCode_ 't') >>= print
  H.Inter.Comm.Para.charAsBox (H.Codes.CharCode_ 'p') >>= print
  H.Inter.Comm.Para.charAsBox (H.Codes.CharCode_ 'y') >>= print

  H.Inter.Comm.MainV.buildMainVList >>= print

testParaAppHLayList :: StateT AppState (ExceptT AppError IO) (Seq (H.Inter.B.List.HListElem, Maybe Hex.Inter.B.List.Para.BreakItem))
testParaAppHLayList = do
  testAppLoadSelectFont
  (_endParaReason, hList) <- H.Inter.Comm.Para.buildParaList DoNotIndent
  pure $ Hex.Inter.B.List.Para.prepareHListForLayout hList

printContainer :: (MonadIO m, Foldable t, Show a, Functor t) => t a -> m ()
printContainer xs = putText $ Tx.intercalate "\n\n" (toList (show <$> xs))

testParaApp :: StateT AppState (ExceptT AppError IO) ()
testParaApp = do
  testAppLoadSelectFont

  (endParaReason, hList) <- H.Inter.Comm.Para.buildParaList DoNotIndent
  print endParaReason
  let layElems = Hex.Inter.B.List.Para.prepareHListForLayout hList
  putText $ Tx.intercalate "\n\n" (toList (show <$> layElems))
