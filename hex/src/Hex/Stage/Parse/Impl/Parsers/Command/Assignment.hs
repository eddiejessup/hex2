module Hex.Stage.Parse.Impl.Parsers.Command.Assignment where

import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.HexState.Interface.Grouped qualified as HSt.Grouped
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hex.Common.Parse.Interface (MonadPrimTokenParse (..))
import Hex.Stage.Parse.Impl.Parsers.Combinators
import Hex.Stage.Parse.Impl.Parsers.Command.Assignment.Macro qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Command.Assignment.NonMacro qualified as Par
import Hex.Stage.Parse.Interface.AST.Command qualified as AST
import Hexlude
import qualified Hex.Common.Parse.Interface as Par

headToParseAssignment :: MonadPrimTokenParse m => PT.PrimitiveToken -> m AST.Assignment
headToParseAssignment = go mempty
  where
    go prefixes = \case
      PT.AssignPrefixTok prefix ->
        anyPrim >>= go (prefixes :|> prefix)
      PT.DefineMacroTok defGlobalType defExpandType -> do
        body <- Par.parseMacroBody defExpandType prefixes
        pure $
          AST.Assignment
            { body,
              scope =
                if defGlobalType == HSt.Grouped.Global || PT.GlobalTok `elem` prefixes
                  then HSt.Grouped.Global
                  else HSt.Grouped.Local
            }
      t -> do
        Log.log "Parsing assignment, did not see prefix"
        body <- Par.headToParseNonMacroAssignmentBody t
        pure $
          AST.Assignment
            { body,
              scope =
                if PT.GlobalTok `elem` prefixes
                  then HSt.Grouped.Global
                  else HSt.Grouped.Local
            }

-- ⟨optional assignments⟩ stands for zero or more ⟨assignment⟩ commands
-- other than \setbox.
parseNonSetBoxAssignment :: MonadPrimTokenParse m => m AST.Assignment
parseNonSetBoxAssignment =
  anyPrim >>= headToParseAssignment >>= \case
    AST.Assignment (AST.SetBoxRegister _ _) _ ->
      Par.parseFailure "parseNonSetBoxAssignment, SetBoxRegister"
    a -> pure a
