module Hex.Stage.Parse.Impl.Parsers.Command.Assignment where

import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.HexState.Interface.Grouped qualified as HSt.Grouped
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hex.Stage.Expand.Interface (PrimTokenSource (..))
import Hex.Stage.Expand.Interface qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Combinators
import Hex.Stage.Parse.Impl.Parsers.Command.Assignment.Macro qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Command.Assignment.NonMacro qualified as Par
import Hex.Stage.Parse.Interface.AST.Command qualified as AST
import Hexlude

headToParseAssignment :: (PrimTokenSource :> es, NonDet :> es, Log.HexLog :> es) => PT.PrimitiveToken -> Eff es AST.Assignment
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
                if defGlobalType == HSt.Grouped.GlobalScope || PT.GlobalTok `elem` prefixes
                  then HSt.Grouped.GlobalScope
                  else HSt.Grouped.LocalScope
            }
      t -> do
        Log.debugLog "Parsing assignment, did not see prefix"
        body <- Par.headToParseNonMacroAssignmentBody t
        pure $
          AST.Assignment
            { body,
              scope =
                if PT.GlobalTok `elem` prefixes
                  then HSt.Grouped.GlobalScope
                  else HSt.Grouped.LocalScope
            }

-- ⟨optional assignments⟩ stands for zero or more ⟨assignment⟩ commands
-- other than \setbox.
parseNonSetBoxAssignment :: (PrimTokenSource :> es, NonDet :> es, Log.HexLog :> es) => Eff es AST.Assignment
parseNonSetBoxAssignment =
  anyPrim >>= headToParseAssignment >>= \case
    AST.Assignment (AST.SetBoxRegister _ _) _ ->
      Par.parseFail "parseNonSetBoxAssignment, SetBoxRegister"
    a -> pure a
