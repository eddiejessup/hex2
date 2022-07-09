{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Parse.Impl where

import Formatting qualified as F
import Hex.Capability.Log.Interface (MonadHexLog)
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.HexState.Interface (MonadHexState)
import Hex.Common.Parse.Impl (fmtParseLog, runParseTMaybe, ParseT)
import Hex.Common.Parse.Interface (ParseUnexpectedError (..))
import Hex.Stage.Expand.Interface qualified as Exp
import Hex.Stage.Lex.Interface qualified as Lex
import Hex.Stage.Parse.Impl.Parsers.Command qualified as Parsers.Command
import Hex.Stage.Parse.Interface
import Hexlude
import qualified Hex.Stage.Parse.Interface.AST.Command as Par
import qualified Hex.Common.Parse.Interface as Par

newtype MonadCommandSourceT m a = MonadCommandSourceT {unMonadCommandSourceT :: m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadState st,
      MonadError e,
      MonadHexState,
      Exp.MonadPrimTokenSource,
      Lex.MonadLexTokenSource,
      MonadHexLog
    )

-- This is quite opaque so to explain:
-- We need (Lex.MonadLexTokenSource m, MonadPrimTokenSource m),
-- because we want 'PrimTokenParse (ParseT m),
-- because that is what we want to run parseCommand in.
-- The instance for 'PrimTokenParse (ParseT m)' requires the above to be true of `m`.
-- So we require that here.
instance
  ( Lex.MonadLexTokenSource (MonadCommandSourceT m),
    Exp.MonadPrimTokenSource (MonadCommandSourceT m),
    MonadError e (MonadCommandSourceT m),
    AsType ParseUnexpectedError e,
    Log.MonadHexLog (MonadCommandSourceT m),
    Monad m
  ) =>
  MonadCommandSource (MonadCommandSourceT m)
  where
  getCommand = getCommandImpl


getCommandImpl ::
  ( Monad m,
    Par.MonadPrimTokenParse (ParseT m),
    MonadError e m,
    AsType ParseUnexpectedError e,
    Log.MonadHexLog m
  ) => m (Maybe Par.Command)
getCommandImpl = do
  (mayCmd, pLog) <- runParseTMaybe Parsers.Command.parseCommand
  Log.log $ F.sformat ("Parsed command: " |%| fmtParseLog) pLog
  pure mayCmd
