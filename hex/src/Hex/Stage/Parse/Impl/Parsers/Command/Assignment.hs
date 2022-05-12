module Hex.Stage.Parse.Impl.Parsers.Command.Assignment where

import Hex.Stage.Parse.Interface.AST.Command qualified as AST
import Hex.Stage.Parse.Impl.Parsers.Combinators qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Command.Assignment.Macro qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Command.Assignment.NonMacro qualified as Par
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as T
import Hexlude
import Hex.Common.Parse.Interface (MonadPrimTokenParse(..))

headToParseAssignment :: MonadPrimTokenParse m => T.PrimitiveToken -> m AST.Assignment
headToParseAssignment = go mempty
  where
    go prefixes = \case
      T.AssignPrefixTok prefix ->
        getAnyPrimitiveToken >>= go (prefixes :|> prefix)
      T.DefineMacroTok defGlobalType defExpandType -> do
        body <- Par.parseMacroBody defExpandType prefixes
        pure $
          AST.Assignment
            { body,
              scope =
                if defGlobalType == T.Global || T.GlobalTok `elem` prefixes
                  then T.Global
                  else T.Local
            }
      t -> do
        body <- Par.headToParseNonMacroAssignmentBody t
        pure $
          AST.Assignment
            { body,
              scope =
                if T.GlobalTok `elem` prefixes
                  then T.Global
                  else T.Local
            }

-- ⟨optional assignments⟩ stands for zero or more ⟨assignment⟩ commands
-- other than \setbox.
parseNonSetBoxAssignment :: MonadPrimTokenParse m => m AST.Assignment
parseNonSetBoxAssignment =
  Par.parseHeaded headToParseAssignment >>= \case
    AST.Assignment (AST.SetBoxRegister _ _) _ ->
      empty
    a -> pure a
