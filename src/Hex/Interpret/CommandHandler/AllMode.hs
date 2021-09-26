module Hex.Interpret.CommandHandler.AllMode where

import Hex.Syntax.Command qualified as H.Syn
import Hex.Syntax.Common qualified as H.Syn
import Hex.Syntax.Font qualified as H.Syn
import Hex.Evaluate.Syntax.Command qualified as H.Ev.Syn
import Hex.Evaluate.Syntax.Quantity ()
import Hex.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Interpret.Build.List.Elem qualified as H.Inter.B.List
import Hex.MonadHexState.Interface qualified as H.St
import Hex.Symbol.Token.Primitive qualified as T
import Hex.Symbol.Token.Resolved qualified as T.Res
import Hexlude

data InterpretError
  = SawEndBoxInMainVMode
  | SawEndBoxInMainVModePara -- "No box to end: in paragraph within main V mode"
  | NoFontSelected
  deriving stock (Generic, Show)

data AllModeCommandResult
  = SawEndBox
  | DidNotSeeEndBox

handleModeIndependentCommand ::
  (Monad m, MonadIO m, H.St.MonadHexState m) =>
  (H.Inter.B.List.VListElem -> m ()) ->
  H.Syn.ModeIndependentCommand 'H.Syn.Evaluated ->
  m AllModeCommandResult
handleModeIndependentCommand addVElem = \case
  H.Syn.WriteMessage (H.Syn.MessageWriteCommand stdStream expandedText) -> do
    let _handle = case stdStream of
          T.StdOut -> stdout
          T.StdErr -> stderr
    liftIO $ hPutStrLn _handle expandedText
    pure DidNotSeeEndBox
  H.Syn.Relax ->
    pure DidNotSeeEndBox
  H.Syn.IgnoreSpaces ->
    pure DidNotSeeEndBox
  -- Re-insert the ⟨token⟩ into the input just after running the next
  -- assignment command. Later \afterassignment commands override earlier
  -- commands. If the assignment is a \setbox, and if the assigned ⟨box⟩ is
  -- \{hbox,vbox,vtop}, insert the ⟨token⟩ just after the '{' in the box
  -- construction (not after the '}'). Insert the ⟨token⟩ just before tokens
  -- inserted by \everyhbox or \everyvbox.
  H.Syn.SetAfterAssignmentToken lt -> do
    H.St.setAfterAssignmentToken (Just lt)
    pure DidNotSeeEndBox
  H.Syn.AddPenalty p -> do
    addVElem $ H.Inter.B.List.ListPenalty $ H.Inter.B.List.Penalty p
    pure DidNotSeeEndBox
  H.Syn.AddKern k -> do
    addVElem $ H.Inter.B.List.VListBaseElem $ H.Inter.B.Box.ElemKern $ H.Inter.B.Box.Kern k
    pure DidNotSeeEndBox
  H.Syn.Assign H.Syn.Assignment {H.Syn.scope, H.Syn.body} -> do
    case body of
      H.Syn.DefineControlSequence cs tgt -> do
        tgtResolvedTok <- case tgt of
          H.Ev.Syn.ResolvedTokenTarget rt ->
            pure rt
          H.Ev.Syn.FontTarget (H.Syn.FontFileSpec fontSpec fontPath) -> do
            fontDefinition <- H.St.loadFont fontPath fontSpec
            addVElem $ H.Inter.B.List.VListBaseElem $ H.Inter.B.Box.ElemFontDefinition fontDefinition
            pure $ T.Res.PrimitiveToken $ T.FontRefToken $ fontDefinition ^. typed @T.FontNumber
        H.St.setSymbol cs scope tgtResolvedTok
      H.Syn.SetVariable ass -> do
        case ass of
          H.Syn.IntVariableAssignment (H.Syn.QuantVariableAssignment var tgt) ->
            H.St.setIntVariable var scope tgt
          H.Syn.LengthVariableAssignment (H.Syn.QuantVariableAssignment var tgt) ->
            H.St.setLengthVariable var scope tgt
          H.Syn.GlueVariableAssignment (H.Syn.QuantVariableAssignment var tgt) ->
            H.St.setGlueVariable var scope tgt
          -- H.Syn.MathGlueVariableAssignment (H.Syn.QuantVariableAssignment var tgt) ->
          --   H.St.setMathGlueVariable var scope tgt
          -- H.Syn.TokenListVariableAssignment (H.Syn.QuantVariableAssignment var tgt) ->
          --   H.St.setTokenListVariable var scope tgt
          H.Syn.SpecialIntParameterAssignment var tgt ->
            H.St.setSpecialIntParameter var tgt
          H.Syn.SpecialLengthParameterAssignment var tgt ->
            H.St.setSpecialLengthParameter var tgt
          _otherVarAss ->
            panic "Not implemented: variable assignment"
      _otherAss ->
        panic "Not implemented: assignment body"
  --       H.Syn.ModifyVariable modCommand ->
  --         case modCommand of
  --           H.Syn.AdvanceIntVariable var plusVal ->
  --             Var.advanceValueFromAST var scope plusVal
  --           H.Syn.AdvanceLengthVariable var plusVal ->
  --             Var.advanceValueFromAST var scope plusVal
  --           H.Syn.AdvanceGlueVariable var plusVal ->
  --             Var.advanceValueFromAST var scope plusVal
  --           H.Syn.AdvanceMathGlueVariable var plusVal ->
  --             Var.advanceValueFromAST var scope plusVal
  --           H.Syn.ScaleVariable vDir numVar scaleVal ->
  --             case numVar of
  --               H.Syn.IntNumericVariable var ->
  --                 Var.scaleValueFromAST var scope vDir scaleVal
  --               H.Syn.LengthNumericVariable var ->
  --                 Var.scaleValueFromAST var scope vDir scaleVal
  --               H.Syn.GlueNumericVariable var ->
  --                 Var.scaleValueFromAST var scope vDir scaleVal
  --               H.Syn.MathGlueNumericVariable var ->
  --                 Var.scaleValueFromAST var scope vDir scaleVal
  --       H.Syn.AssignCode (H.Syn.CodeAssignment (H.Syn.CodeTableRef codeType idx) val) ->
  --         do
  --           eIdx <- texEvaluate idx
  --           eVal <- texEvaluate val
  --           idxChar <- note (injectTyped $ ConfigError $ "Invalid character code index: " <> show eIdx) (fromHexInt eIdx)
  --           sLogStampedJSON
  --             "Doing code assignment"
  --             [ ("codeTableIndexSymbolic", toJSON idx),
  --               ("codeTableIndexEvaluated", toJSON eIdx),
  --               ("codeTableIndexAsChar", toJSON idxChar),
  --               ("codeTableValueSymbolic", toJSON val),
  --               ("codeTableValueEvaluated", toJSON eVal),
  --               ("codeType", toJSON codeType),
  --               ("scope", toJSON scope)
  --             ]
  --           updateCharCodeMap codeType idxChar eVal scope
  --       H.Syn.SelectFont fNr ->
  --         do
  --           selectFont fNr scope
  --           addVElem $ H.Inter.B.List.VListBaseElem $ H.Inter.B.Box.ElemFontSelection $ H.Inter.B.Box.FontSelection fNr
  --       H.Syn.SetFamilyMember fm fontRef ->
  --         do
  --           eFm <- texEvaluate fm
  --           fNr <- texEvaluate fontRef
  --           modifying' (typed @Config) $ setFamilyMemberFont eFm fNr scope
  --       -- Start a new level of grouping. Enter inner mode.
  --       H.Syn.SetBoxRegister lhsIdx box ->
  --         do
  --           eLhsIdx <- texEvaluate lhsIdx
  --           case box of
  --             H.Syn.FetchedRegisterBox fetchMode rhsIdx ->
  --               do
  --                 fetchedMaybeBox <- fetchBox fetchMode rhsIdx
  --                 modifying' (typed @Config) $ setBoxRegisterNullable eLhsIdx scope fetchedMaybeBox
  --             H.Syn.LastBox ->
  --               panic "Not implemented: SetBoxRegister to LastBox"
  --             H.Syn.VSplitBox _ _ ->
  --               panic "Not implemented: SetBoxRegister to VSplitBox"
  --             H.Syn.ExplicitBox spec boxType -> do
  --               eSpec <- texEvaluate spec
  --               modifying' (typed @Config) $ pushGroup (ScopeGroup newLocalScope ExplicitBoxGroup)
  --               extractedBox <- extractExplicitBox eSpec boxType
  --               modifying' (typed @Config) $ setBoxRegister eLhsIdx extractedBox scope
  --       H.Syn.SetFontChar (H.Syn.FontCharRef fontChar fontRef) charRef ->
  --         do
  --           fNr <- texEvaluate fontRef
  --           eCharRef <- texEvaluate charRef
  --           let updateFontChar f = case fontChar of
  --                 H.Syn.SkewChar -> f {skewChar = eCharRef}
  --                 H.Syn.HyphenChar -> f {hyphenChar = eCharRef}
  --           modifyFont fNr updateFontChar
  --       oth ->
  --         panic $ show oth
  --     use (typed @Config % #afterAssignmentToken) >>= \case
  --       Nothing ->
  --         pure ()
  --       Just lt ->
  --         do
  --           insertLexToken lt
  --           assign' (typed @Config % #afterAssignmentToken) Nothing
    pure DidNotSeeEndBox
  -- H.Syn.WriteToStream n (H.Syn.ImmediateWriteText eTxt) -> do
  --   en <- texEvaluate n
  --   fStreams <- use $ typed @Config % #outFileStreams
  --   let txtTxt = Codes.unsafeCodesAsChars (showExpandedBalancedText eTxt)
  --   -- Write to:
  --   -- if stream number corresponds to existing, open file:
  --   --     file
  --   -- otherwise:
  --   --     log
  --   --     unless stream number is negative: terminal
  --   case getFileStream fStreams en of
  --     Just fStream ->
  --       liftIO $ hPutStrLn fStream txtTxt
  --     Nothing ->
  --       do
  --         -- Write to terminal.
  --         when (en >= 0) $ sLog (BS.C8.pack txtTxt)
  --         -- Write to log
  --         logHandle <- use $ typed @Config % #logStream
  --         liftIO $ hPutStrLn logHandle txtTxt
  --   pure DidNotSeeEndBox
  -- -- Start a new level of grouping.
  -- H.Syn.ChangeScope H.Syn.Positive entryTrig ->
  --   do
  --     modifying' (typed @Config) $ pushGroup (ScopeGroup newLocalScope (LocalStructureGroup entryTrig))
  --     pure DidNotSeeEndBox
  -- -- Do the appropriate finishing actions, undo the
  -- -- effects of non-global assignments, and leave the
  -- -- group. Maybe leave the current mode.
  -- H.Syn.ChangeScope H.Syn.Negative exitTrig -> do
  --   prePopCurrentFontNr <- uses (typed @Config) lookupCurrentFontNr
  --   (group, poppedConfig) <- uses (typed @Config) popGroup >>= \case
  --     Nothing ->
  --       throwError $ injectTyped $ ConfigError "No group to leave"
  --     Just v ->
  --       pure v
  --   assign' (typed @Config) poppedConfig
  --   postPopCurrentFontNr <- uses (typed @Config) lookupCurrentFontNr
  --   when (prePopCurrentFontNr /= postPopCurrentFontNr) $ do
  --     sLogStampedJSON
  --       "After exiting scope, reverting changed font"
  --       [ ("fontNrPrePop", toJSON prePopCurrentFontNr),
  --         ("fontNrPostPop", toJSON postPopCurrentFontNr),
  --         ("groupType", toJSON (renderGroupType group))
  --       ]
  --     addVElem $ H.Inter.B.List.VListBaseElem $ H.Inter.B.Box.ElemFontSelection $ H.Inter.B.Box.FontSelection (fromMaybe 0 postPopCurrentFontNr)
  --   case group of
  --     -- Undo the effects of non-global
  --     -- assignments without leaving the
  --     -- current mode.
  --     ScopeGroup _ (LocalStructureGroup entryTrig) -> do
  --       when (entryTrig /= exitTrig)
  --         $ throwError
  --         $ injectTyped
  --         $ ConfigError
  --         $ "Entry and exit group triggers differ: " <> show (exitTrig, entryTrig)
  --       pure DidNotSeeEndBox
  --     -- - Undo the effects of non-global assignments
  --     -- - package the [box] using the size that was saved on the
  --     --   stack
  --     -- - complete the \setbox command
  --     -- - return to the mode we were in at the time of the
  --     --   \setbox.
  --     ScopeGroup _ ExplicitBoxGroup ->
  --       pure SawEndBox
  --     NonScopeGroup ->
  --       pure DidNotSeeEndBox
  -- H.Syn.AddBox H.Syn.NaturalPlacement boxSource -> do
  --   case boxSource of
  --     H.Syn.FetchedRegisterBox fetchMode idx ->
  --       fetchBox fetchMode idx >>= \case
  --         Nothing ->
  --           pure ()
  --         Just b ->
  --           addVElem $ H.Inter.B.List.VListBaseElem $ H.Inter.B.Box.ElemBox b
  --     H.Syn.ExplicitBox spec boxType -> do
  --       -- Start a new level of grouping. Enter inner mode.
  --       eSpec <- texEvaluate spec
  --       modifying' (typed @Config) $ pushGroup (ScopeGroup newLocalScope ExplicitBoxGroup)
  --       b <- extractExplicitBox eSpec boxType
  --       addVElem $ H.Inter.B.List.VListBaseElem $ H.Inter.B.Box.ElemBox b
  --   pure DidNotSeeEndBox
  _oth ->
    panic "All-mode command not implemented"
