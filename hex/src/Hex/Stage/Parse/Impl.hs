{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Parse.Impl where

import Hex.Capability.Log.Interface (MonadHexLog)
import Hex.Common.HexState.Interface (MonadHexState)
import Hex.Common.Parse.Impl (runParseTMaybe)
import Hex.Common.Parse.Interface (ParseUnexpectedError (..))
import Hex.Stage.Expand.Interface qualified as Exp
import Hex.Stage.Lex.Interface qualified as Lex
import Hex.Stage.Parse.Impl.Parsers.Command qualified as Parsers.Command
import Hex.Stage.Parse.Interface
import Hexlude

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
  ( Monad (MonadCommandSourceT m),
    Lex.MonadLexTokenSource (MonadCommandSourceT m),
    Exp.MonadPrimTokenSource (MonadCommandSourceT m),
    MonadError e (MonadCommandSourceT m),
    AsType ParseUnexpectedError e,
    MonadHexState (MonadCommandSourceT m)
  ) =>
  MonadCommandSource (MonadCommandSourceT m)
  where
  getCommand = runParseTMaybe Parsers.Command.parseCommand
