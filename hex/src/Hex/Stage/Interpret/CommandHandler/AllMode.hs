module Hex.Stage.Interpret.CommandHandler.AllMode where

import Hex.Common.HexState.Interface qualified as H.St
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Resolve qualified as Res
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as T
import Hex.Stage.Evaluate.Interface.AST.Command qualified as AST
import Hex.Stage.Evaluate.Interface.AST.Command qualified as Eval
import Hex.Stage.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Stage.Interpret.Build.List.Elem qualified as H.Inter.B.List
import Hexlude

data InterpretError
  = SawEndBoxInMainVMode
  | SawEndBoxInMainVModePara -- "No box to end: in paragraph within main V mode"
  | NoFontSelected
  | UnexpectedEndOfInput
  deriving stock (Generic, Show)

data AllModeCommandResult
  = SawEndBox
  | DidNotSeeEndBox

handleModeIndependentCommand ::
  (Monad m, MonadIO m, HSt.MonadHexState m) =>
  (H.Inter.B.List.VListElem -> m ()) ->
  AST.ModeIndependentCommand ->
  m AllModeCommandResult
handleModeIndependentCommand addVElem = \case
  Eval.WriteMessage (Eval.MessageWriteCommand stdStream expandedText) -> do
    let _handle = case stdStream of
          T.StdOut -> stdout
          T.StdErr -> stderr
    liftIO $ hPutStrLn _handle expandedText
    pure DidNotSeeEndBox
  Eval.Relax ->
    pure DidNotSeeEndBox
  Eval.IgnoreSpaces ->
    pure DidNotSeeEndBox
  -- Re-insert the ⟨token⟩ into the input just after running the next
  -- assignment command. Later \afterassignment commands override earlier
  -- commands. If the assignment is a \setbox, and if the assigned ⟨box⟩ is
  -- \{hbox,vbox,vtop}, insert the ⟨token⟩ just after the '{' in the box
  -- construction (not after the '}'). Insert the ⟨token⟩ just before tokens
  -- inserted by \everyhbox or \everyvbox.
  Eval.SetAfterAssignmentToken lt -> do
    H.St.setAfterAssignmentToken (Just lt)
    pure DidNotSeeEndBox
  Eval.AddPenalty p -> do
    addVElem $ H.Inter.B.List.ListPenalty p
    pure DidNotSeeEndBox
  Eval.AddKern k -> do
    addVElem $ H.Inter.B.List.VListBaseElem $ H.Inter.B.Box.ElemKern k
    pure DidNotSeeEndBox
  Eval.Assign Eval.Assignment {Eval.scope, Eval.body} -> do
    case body of
      Eval.DefineControlSequence cs tgt -> do
        tgtResolvedTok <- case tgt of
          Eval.NonFontTarget rt ->
            pure rt
          Eval.FontTarget (Eval.FontFileSpec fontSpec fontPath) -> do
            fontDefinition <- H.St.loadFont fontPath fontSpec
            addVElem $ H.Inter.B.List.VListBaseElem $ H.Inter.B.Box.ElemFontDefinition fontDefinition
            pure $ Res.PrimitiveToken $ T.FontRefToken $ fontDefinition ^. typed @T.FontNumber
        H.St.setControlSequence cs tgtResolvedTok scope
        pure DidNotSeeEndBox
  --       AST.SetVariable ass ->
  --         case ass of
  --           AST.TeXIntVariableAssignment v tgt ->
  --             Var.setValueFromAST v scope tgt
  --           AST.LengthVariableAssignment v tgt ->
  --             Var.setValueFromAST v scope tgt
  --           AST.GlueVariableAssignment v tgt ->
  --             Var.setValueFromAST v scope tgt
  --           AST.MathGlueVariableAssignment v tgt ->
  --             Var.setValueFromAST v scope tgt
  --           AST.TokenListVariableAssignment v tgt ->
  --             Var.setValueFromAST v scope tgt
  --           AST.SpecialTeXIntVariableAssignment v tgt ->
  --             Var.setValueFromAST v scope tgt
  --           AST.SpecialLengthParameterVariableAssignment v tgt ->
  --             Var.setValueFromAST v scope tgt
  --       AST.ModifyVariable modCommand ->
  --         case modCommand of
  --           AST.AdvanceTeXIntVariable var plusVal ->
  --             Var.advanceValueFromAST var scope plusVal
  --           AST.AdvanceLengthVariable var plusVal ->
  --             Var.advanceValueFromAST var scope plusVal
  --           AST.AdvanceGlueVariable var plusVal ->
  --             Var.advanceValueFromAST var scope plusVal
  --           AST.AdvanceMathGlueVariable var plusVal ->
  --             Var.advanceValueFromAST var scope plusVal
  --           AST.ScaleVariable vDir numVar scaleVal ->
  --             case numVar of
  --               AST.TeXIntNumericVariable var ->
  --                 Var.scaleValueFromAST var scope vDir scaleVal
  --               AST.LengthNumericVariable var ->
  --                 Var.scaleValueFromAST var scope vDir scaleVal
  --               AST.GlueNumericVariable var ->
  --                 Var.scaleValueFromAST var scope vDir scaleVal
  --               AST.MathGlueNumericVariable var ->
  --                 Var.scaleValueFromAST var scope vDir scaleVal
  --       AST.AssignCode (AST.CodeAssignment (AST.CodeTableRef codeType idx) val) ->
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
  --       AST.SelectFont fNr ->
  --         do
  --           selectFont fNr scope
  --           addVElem $ H.Inter.B.List.VListBaseElem $ H.Inter.B.Box.ElemFontSelection $ H.Inter.B.Box.FontSelection fNr
  --       AST.SetFamilyMember fm fontRef ->
  --         do
  --           eFm <- texEvaluate fm
  --           fNr <- texEvaluate fontRef
  --           modifying' (typed @Config) $ setFamilyMemberFont eFm fNr scope
  --       -- Start a new level of grouping. Enter inner mode.
  --       AST.SetBoxRegister lhsIdx box ->
  --         do
  --           eLhsIdx <- texEvaluate lhsIdx
  --           case box of
  --             AST.FetchedRegisterBox fetchMode rhsIdx ->
  --               do
  --                 fetchedMaybeBox <- fetchBox fetchMode rhsIdx
  --                 modifying' (typed @Config) $ setBoxRegisterNullable eLhsIdx scope fetchedMaybeBox
  --             AST.LastBox ->
  --               panic "Not implemented: SetBoxRegister to LastBox"
  --             AST.VSplitBox _ _ ->
  --               panic "Not implemented: SetBoxRegister to VSplitBox"
  --             AST.ExplicitBox spec boxType -> do
  --               eSpec <- texEvaluate spec
  --               modifying' (typed @Config) $ pushGroup (ScopeGroup newLocalScope ExplicitBoxGroup)
  --               extractedBox <- extractExplicitBox eSpec boxType
  --               modifying' (typed @Config) $ setBoxRegister eLhsIdx extractedBox scope
  --       AST.SetFontChar (AST.FontCharRef fontChar fontRef) charRef ->
  --         do
  --           fNr <- texEvaluate fontRef
  --           eCharRef <- texEvaluate charRef
  --           let updateFontChar f = case fontChar of
  --                 AST.SkewChar -> f {skewChar = eCharRef}
  --                 AST.HyphenChar -> f {hyphenChar = eCharRef}
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
  --     pure DidNotSeeEndBox
  -- AST.WriteToStream n (AST.ImmediateWriteText eTxt) -> do
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
  -- AST.ChangeScope AST.Positive entryTrig ->
  --   do
  --     modifying' (typed @Config) $ pushGroup (ScopeGroup newLocalScope (LocalStructureGroup entryTrig))
  --     pure DidNotSeeEndBox
  -- -- Do the appropriate finishing actions, undo the
  -- -- effects of non-global assignments, and leave the
  -- -- group. Maybe leave the current mode.
  -- AST.ChangeScope AST.Negative exitTrig -> do
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
  -- AST.AddBox AST.NaturalPlacement boxSource -> do
  --   case boxSource of
  --     AST.FetchedRegisterBox fetchMode idx ->
  --       fetchBox fetchMode idx >>= \case
  --         Nothing ->
  --           pure ()
  --         Just b ->
  --           addVElem $ H.Inter.B.List.VListBaseElem $ H.Inter.B.Box.ElemBox b
  --     AST.ExplicitBox spec boxType -> do
  --       -- Start a new level of grouping. Enter inner mode.
  --       eSpec <- texEvaluate spec
  --       modifying' (typed @Config) $ pushGroup (ScopeGroup newLocalScope ExplicitBoxGroup)
  --       b <- extractExplicitBox eSpec boxType
  --       addVElem $ H.Inter.B.List.VListBaseElem $ H.Inter.B.Box.ElemBox b
  --   pure DidNotSeeEndBox
  oth ->
    panic $ show oth
