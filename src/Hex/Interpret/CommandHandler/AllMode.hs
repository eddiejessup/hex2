module Hex.Interpret.CommandHandler.AllMode where

import Hex.Parse.AST qualified as H.AST
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
  ( Monad m
  ) =>
  H.AST.ModeIndependentCommand ->
  m AllModeCommandResult
handleModeIndependentCommand = \case
  -- H.AST.Message stdOutStream eTxt -> do
  --   let _handle = case stdOutStream of
  --         H.Sym.Tok.StdOut -> stdout
  --         H.Sym.Tok.StdErr -> stderr
  --   liftIO $ hPutStrLn _handle (toS (unsafeCodesAsChars (showExpandedBalancedText eTxt)) :: Text)
  --   pure DidNotSeeEndBox
  H.AST.Relax ->
    pure DidNotSeeEndBox
  H.AST.IgnoreSpaces ->
    pure DidNotSeeEndBox
  -- Re-insert the ⟨token⟩ into the input just after running the next
  -- assignment command. Later \afterassignment commands override earlier
  -- commands. If the assignment is a \setbox, and if the assigned ⟨box⟩ is
  -- \{hbox,vbox,vtop}, insert the ⟨token⟩ just after the '{' in the box
  -- construction (not after the '}'). Insert the ⟨token⟩ just before tokens
  -- inserted by \everyhbox or \everyvbox.
  -- H.AST.SetAfterAssignmentToken lt -> do
  --   assign' (typed @Config % field @"afterAssignmentToken") (Just lt)
  --   pure DidNotSeeEndBox
  -- H.AST.AddPenalty n -> do
  --   addVElem . BL.ListPenalty . BL.Penalty =<< texEvaluate n
  --   pure DidNotSeeEndBox
  -- H.AST.AddKern ln -> do
  --   addVElem . BL.VListBaseElem . B.ElemKern . B.Kern =<< texEvaluate ln
  --   pure DidNotSeeEndBox
  -- H.AST.Assign H.AST.Assignment {H.AST.scope, H.AST.body} ->
  --   do
  --     case body of
  --       H.AST.DefineControlSequence cs tgt ->
  --         do
  --           newCSTok <- case tgt of
  --             H.AST.MacroTarget macro ->
  --               pure (HR.syntaxTok $ H.AST.MacroTok macro)
  --             -- TODO: If a \let target is an active character, should we
  --             -- treat it as a control sequence, or a char-cat pair?
  --             H.AST.LetTarget (Lex.CharCatToken tgtCC) ->
  --               pure (HR.primTok $ H.AST.LetCharCat tgtCC)
  --             H.AST.LetTarget (Lex.ControlSequenceToken tgtCS) ->
  --               do
  --                 mayCS <- uses (typed @Config) (lookupCSProper tgtCS)
  --                 let resTok = fromMaybe (H.AST.PrimitiveToken H.AST.RelaxTok) mayCS
  --                 pure resTok
  --             H.AST.ShortDefineTarget q n ->
  --               do
  --                 en <- texEvaluate n
  --                 pure (HR.primTok $ H.AST.IntRefTok q en)
  --             H.AST.FontTarget fontSpec fPath ->
  --               do
  --                 fontDef@B.FontDefinition {B.fontNr} <- loadFont fPath fontSpec
  --                 addVElem $ BL.VListBaseElem $ B.ElemFontDefinition fontDef
  --                 pure $ HR.primTok $ H.AST.FontRefToken fontNr
  --             oth ->
  --               panic $ "Not implemented: DefineControlSequence target " <> show oth
  --           sLogStampedJSON
  --             "Defining control sequence"
  --             [ ("controlSequence", toJSON cs),
  --               ("token", toJSON newCSTok),
  --               ("scope", toJSON scope)
  --             ]
  --           modifying' (typed @Config) $ setControlSequence cs newCSTok scope
  --       H.AST.SetVariable ass ->
  --         case ass of
  --           H.AST.TeXIntVariableAssignment v tgt ->
  --             Var.setValueFromAST v scope tgt
  --           H.AST.LengthVariableAssignment v tgt ->
  --             Var.setValueFromAST v scope tgt
  --           H.AST.GlueVariableAssignment v tgt ->
  --             Var.setValueFromAST v scope tgt
  --           H.AST.MathGlueVariableAssignment v tgt ->
  --             Var.setValueFromAST v scope tgt
  --           H.AST.TokenListVariableAssignment v tgt ->
  --             Var.setValueFromAST v scope tgt
  --           H.AST.SpecialTeXIntVariableAssignment v tgt ->
  --             Var.setValueFromAST v scope tgt
  --           H.AST.SpecialLengthParameterVariableAssignment v tgt ->
  --             Var.setValueFromAST v scope tgt
  --       H.AST.ModifyVariable modCommand ->
  --         case modCommand of
  --           H.AST.AdvanceTeXIntVariable var plusVal ->
  --             Var.advanceValueFromAST var scope plusVal
  --           H.AST.AdvanceLengthVariable var plusVal ->
  --             Var.advanceValueFromAST var scope plusVal
  --           H.AST.AdvanceGlueVariable var plusVal ->
  --             Var.advanceValueFromAST var scope plusVal
  --           H.AST.AdvanceMathGlueVariable var plusVal ->
  --             Var.advanceValueFromAST var scope plusVal
  --           H.AST.ScaleVariable vDir numVar scaleVal ->
  --             case numVar of
  --               H.AST.TeXIntNumericVariable var ->
  --                 Var.scaleValueFromAST var scope vDir scaleVal
  --               H.AST.LengthNumericVariable var ->
  --                 Var.scaleValueFromAST var scope vDir scaleVal
  --               H.AST.GlueNumericVariable var ->
  --                 Var.scaleValueFromAST var scope vDir scaleVal
  --               H.AST.MathGlueNumericVariable var ->
  --                 Var.scaleValueFromAST var scope vDir scaleVal
  --       H.AST.AssignCode (H.AST.CodeAssignment (H.AST.CodeTableRef codeType idx) val) ->
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
  --       H.AST.SelectFont fNr ->
  --         do
  --           selectFont fNr scope
  --           addVElem $ BL.VListBaseElem $ B.ElemFontSelection $ B.FontSelection fNr
  --       H.AST.SetFamilyMember fm fontRef ->
  --         do
  --           eFm <- texEvaluate fm
  --           fNr <- texEvaluate fontRef
  --           modifying' (typed @Config) $ setFamilyMemberFont eFm fNr scope
  --       -- Start a new level of grouping. Enter inner mode.
  --       H.AST.SetBoxRegister lhsIdx box ->
  --         do
  --           eLhsIdx <- texEvaluate lhsIdx
  --           case box of
  --             H.AST.FetchedRegisterBox fetchMode rhsIdx ->
  --               do
  --                 fetchedMaybeBox <- fetchBox fetchMode rhsIdx
  --                 modifying' (typed @Config) $ setBoxRegisterNullable eLhsIdx scope fetchedMaybeBox
  --             H.AST.LastBox ->
  --               panic "Not implemented: SetBoxRegister to LastBox"
  --             H.AST.VSplitBox _ _ ->
  --               panic "Not implemented: SetBoxRegister to VSplitBox"
  --             H.AST.ExplicitBox spec boxType -> do
  --               eSpec <- texEvaluate spec
  --               modifying' (typed @Config) $ pushGroup (ScopeGroup newLocalScope ExplicitBoxGroup)
  --               extractedBox <- extractExplicitBox eSpec boxType
  --               modifying' (typed @Config) $ setBoxRegister eLhsIdx extractedBox scope
  --       H.AST.SetFontChar (H.AST.FontCharRef fontChar fontRef) charRef ->
  --         do
  --           fNr <- texEvaluate fontRef
  --           eCharRef <- texEvaluate charRef
  --           let updateFontChar f = case fontChar of
  --                 H.AST.SkewChar -> f {skewChar = eCharRef}
  --                 H.AST.HyphenChar -> f {hyphenChar = eCharRef}
  --           modifyFont fNr updateFontChar
  --       oth ->
  --         panic $ show oth
  --     use (typed @Config % field @"afterAssignmentToken") >>= \case
  --       Nothing ->
  --         pure ()
  --       Just lt ->
  --         do
  --           insertLexToken lt
  --           assign' (typed @Config % field @"afterAssignmentToken") Nothing
  --     pure DidNotSeeEndBox
  -- H.AST.WriteToStream n (H.AST.ImmediateWriteText eTxt) -> do
  --   en <- texEvaluate n
  --   fStreams <- use $ typed @Config % field @"outFileStreams"
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
  --         logHandle <- use $ typed @Config % field @"logStream"
  --         liftIO $ hPutStrLn logHandle txtTxt
  --   pure DidNotSeeEndBox
  -- -- Start a new level of grouping.
  -- H.AST.ChangeScope H.AST.Positive entryTrig ->
  --   do
  --     modifying' (typed @Config) $ pushGroup (ScopeGroup newLocalScope (LocalStructureGroup entryTrig))
  --     pure DidNotSeeEndBox
  -- -- Do the appropriate finishing actions, undo the
  -- -- effects of non-global assignments, and leave the
  -- -- group. Maybe leave the current mode.
  -- H.AST.ChangeScope H.AST.Negative exitTrig -> do
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
  --     addVElem $ BL.VListBaseElem $ B.ElemFontSelection $ B.FontSelection (fromMaybe 0 postPopCurrentFontNr)
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
  -- H.AST.AddBox H.AST.NaturalPlacement boxSource -> do
  --   case boxSource of
  --     H.AST.FetchedRegisterBox fetchMode idx ->
  --       fetchBox fetchMode idx >>= \case
  --         Nothing ->
  --           pure ()
  --         Just b ->
  --           addVElem $ BL.VListBaseElem $ B.ElemBox b
  --     H.AST.ExplicitBox spec boxType -> do
  --       -- Start a new level of grouping. Enter inner mode.
  --       eSpec <- texEvaluate spec
  --       modifying' (typed @Config) $ pushGroup (ScopeGroup newLocalScope ExplicitBoxGroup)
  --       b <- extractExplicitBox eSpec boxType
  --       addVElem $ BL.VListBaseElem $ B.ElemBox b
  --   pure DidNotSeeEndBox
  oth ->
    panic $ show oth
