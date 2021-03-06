module Hex.App where

import Protolude
import Hex.Lex.Types qualified as H.Lex
import Hex.Interpret.CommandHandler.MainVMode qualified as H.Inter.Comm.MainV
import Hex.HexState.Type qualified as H.St
import Hex.Parse.CharSource qualified as H.Par.ChrSrc
import Hex.Parse.MonadParse.Impls.MonadTokenSource qualified as H.Par.Par

data AppState = AppState H.St.HexState H.Par.ChrSrc.CharSource
  deriving stock Generic

newAppState :: ByteString -> AppState
newAppState chrs = AppState H.St.newHexState (H.Par.ChrSrc.newCharSource chrs)

data AppError
  = AppLexError H.Lex.LexError
  | AppParseError H.Par.Par.ParseError
  | AppInterpretError H.Inter.Comm.MainV.InterpretError
  deriving stock (Generic, Show)

runApp ::
  AppState ->
  StateT AppState (ExceptT AppError Identity) a ->
  Either AppError (a, AppState)
runApp chrSrc st = runIdentity (runExceptT (runStateT st chrSrc))

runNewApp ::
  ByteString ->
  StateT AppState (ExceptT AppError Identity) a ->
  Either AppError (a, AppState)
runNewApp = runApp . newAppState

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
evalNewApp ::
  ByteString ->
  StateT AppState (ExceptT AppError Identity) a ->
  Either AppError a
evalNewApp chrs = fmap fst . runNewApp chrs
