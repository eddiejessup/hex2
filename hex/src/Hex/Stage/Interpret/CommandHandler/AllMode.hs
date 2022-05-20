module Hex.Stage.Interpret.CommandHandler.AllMode where

import Formatting qualified as F
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Grouped qualified as HSt.Group
import Hex.Common.HexState.Interface.Resolve qualified as Res
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as ST
import Hex.Common.HexState.Interface.Variable qualified as HSt.Var
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Evaluate.Interface qualified as Eval
import Hex.Stage.Evaluate.Interface.AST.Command qualified as Eval
import Hex.Stage.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Stage.Interpret.Build.List.Elem qualified as H.Inter.B.List
import Hex.Stage.Lex.Interface qualified as Lex
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Parse.Interface qualified as Par
import Hexlude

data InterpretError
  = SawEndBoxInMainVMode
  | SawEndBoxInMainVModePara -- "No box to end: in paragraph within main V mode"
  | NoFontSelected
  | UnexpectedEndOfInput
  deriving stock (Show, Generic)

fmtInterpretError :: Fmt InterpretError
fmtInterpretError = F.shown

data AllModeCommandResult
  = SawEndBox
  | DidNotSeeEndBox

getNextCommandLogged ::
  ( Eval.MonadEvaluate m,
    Par.MonadCommandSource m,
    MonadError e m,
    AsType InterpretError e
  ) =>
  m Eval.Command
getNextCommandLogged = do
  cmd <- Eval.getEvalCommandErrorEOF $ injectTyped UnexpectedEndOfInput
  -- Log.log $ F.sformat ("Read command: " |%| F.shown) cmd
  pure cmd

data OutputDestination
  = FileStream Q.HexInt
  | StandardStream PT.StandardOutputSource

writeToOutput :: Monad m => OutputDestination -> Text -> m ()
writeToOutput _ _ = pure ()

-- let _handle = case stdStream of
--       PT.StdOut -> stdout
--       PT.StdErr -> stderr
-- liftIO $ hPutStrLn _handle expandedText
-- fStreams <- use $ typed @Config % #outFileStreams
-- let txtTxt = Codes.unsafeCodesAsChars (showExpandedBalancedText eTxt)
-- If stream number corresponds to an existing, open file:
--     write to file
-- Otherwise:
--     - Write to log
--     - If stream number is >= 0: also write to terminal
-- case getFileStream fStreams en of
--   Just fStream ->
--     liftIO $ hPutStrLn fStream txtTxt
--   Nothing -> do
-- -- Write to terminal.
-- when (en >= 0) $ sLog (BS.C8.pack txtTxt)
-- -- Write to log
-- logHandle <- use $ typed @Config % #logStream
-- liftIO $ hPutStrLn logHandle txtTxt

handleModeIndependentCommand ::
  ( Monad m,
    HSt.MonadHexState m,
    Lex.MonadLexTokenSource m,
    Log.MonadHexLog m
  ) =>
  (H.Inter.B.List.VListElem -> StateT s m ()) ->
  Eval.ModeIndependentCommand ->
  StateT s m AllModeCommandResult
handleModeIndependentCommand addVElem = \case
  Eval.DebugShowState -> do
    lift $ Log.logInternalState
    pure DidNotSeeEndBox
  Eval.WriteMessage (Eval.MessageWriteCommand stdStream expandedText) -> do
    writeToOutput (StandardStream stdStream) expandedText
    pure DidNotSeeEndBox
  Eval.WriteToStream (Eval.StreamWriteCommand n writeText) -> do
    case writeText of
      Eval.ImmediateWriteText txt -> do
        writeToOutput (FileStream n) txt
      Eval.DeferredWriteText _ ->
        notImplemented "Write to stream: DeferredWriteText"
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
    HSt.setAfterAssignmentToken lt
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
        maySymbolTarget <- case tgt of
          Eval.MacroTarget macroDefinition -> do
            pure $ Just $ Res.SyntaxCommandHeadToken $ ST.MacroTok macroDefinition
          Eval.LetTarget lexTokenTargetValue ->
            case lexTokenTargetValue of
              -- If the target of the \let is a char-cat pair, then resolve the
              -- new control sequence to a 'let-char-cat' containing that token.
              Lex.CharCatLexToken charCat ->
                pure $ Just $ Res.PrimitiveToken $ PT.LetCharCat charCat
              -- If the target is a control sequence, then try to resolve that symbol.
              Lex.ControlSequenceLexToken controlSequence ->
                HSt.resolveSymbol (Res.ControlSequenceSymbol controlSequence) >>= \case
                  -- If the target symbol does not exist, then do nothing, i.e.
                  -- assign no new control sequence. (I am basing this on the
                  -- behaviour of tex-the-program.)
                  Nothing -> pure Nothing
                  -- If the target symbol does exist, then create a new symbol
                  -- entry with the same contents as the target symbol.
                  Just symbol -> pure $ Just symbol
          Eval.FutureLetTarget _futureLetDefinition -> do
            notImplemented "Define control sequence: future let"
          Eval.ShortDefineTarget charryQuantityType targetValue -> do
            pure $ Just $ Res.PrimitiveToken $ PT.IntRefTok charryQuantityType targetValue
          Eval.ReadTarget _readInt -> do
            notImplemented "ReadTarget"
          Eval.FontTarget (Eval.FontFileSpec fontSpec fontPath) -> do
            fontDefinition <- HSt.loadFont fontPath fontSpec
            addVElem $ H.Inter.B.List.VListBaseElem $ H.Inter.B.Box.ElemFontDefinition fontDefinition
            pure $ Just $ Res.PrimitiveToken $ PT.FontRefToken $ fontDefinition ^. typed @PT.FontNumber
        case maySymbolTarget of
          Nothing ->
            pure ()
          Just symbolTarget ->
            HSt.setSymbol cs symbolTarget scope
      Eval.AssignCode (Eval.CodeAssignment idxChar codeVal) ->
        case codeVal of
          Eval.CatCodeValue catCode ->
            HSt.setHexCode Code.CCatCodeType idxChar catCode scope
          Eval.MathCodeValue mathCode ->
            HSt.setHexCode Code.CMathCodeType idxChar mathCode scope
          Eval.UpperCaseCodeValue upperCaseCode ->
            HSt.setHexCode Code.CUpperCaseCodeType idxChar upperCaseCode scope
          Eval.LowerCaseCodeValue lowerCaseCode ->
            HSt.setHexCode Code.CLowerCaseCodeType idxChar lowerCaseCode scope
          Eval.SpaceFactorCodeValue spaceFactorCode ->
            HSt.setHexCode Code.CSpaceFactorCodeType idxChar spaceFactorCode scope
          Eval.DelimiterCodeValue delimiterCode ->
            HSt.setHexCode Code.CDelimiterCodeType idxChar delimiterCode scope
      Eval.SetVariable ass ->
        case ass of
          Eval.IntVariableAssignment (Eval.QuantVariableAssignment var tgt) ->
            case var of
              HSt.Var.ParamVar intParam ->
                HSt.setParameterValue intParam tgt scope
              HSt.Var.RegisterVar registerLoc ->
                HSt.setRegisterValue registerLoc tgt scope
          Eval.LengthVariableAssignment (Eval.QuantVariableAssignment var tgt) ->
            case var of
              HSt.Var.ParamVar lengthParam ->
                HSt.setParameterValue lengthParam tgt scope
              HSt.Var.RegisterVar registerLoc ->
                HSt.setRegisterValue registerLoc tgt scope
          Eval.GlueVariableAssignment (Eval.QuantVariableAssignment var tgt) ->
            case var of
              HSt.Var.ParamVar glueParam ->
                HSt.setParameterValue glueParam tgt scope
              HSt.Var.RegisterVar registerLoc ->
                HSt.setRegisterValue registerLoc tgt scope
          Eval.MathGlueVariableAssignment (Eval.QuantVariableAssignment var tgt) ->
            case var of
              HSt.Var.ParamVar mathGlueParam ->
                HSt.setParameterValue mathGlueParam tgt scope
              HSt.Var.RegisterVar registerLoc ->
                HSt.setRegisterValue registerLoc tgt scope
          Eval.TokenListVariableAssignment (Eval.QuantVariableAssignment var _tgt) ->
            case var of
              HSt.Var.ParamVar _tokenListParam ->
                notImplemented "TokenListVariableAssignment, parameter-variable"
              HSt.Var.RegisterVar _registerLoc ->
                notImplemented "TokenListVariableAssignment, register-variable"
          Eval.SpecialIntParameterVariableAssignment param tgt ->
            HSt.setSpecialIntParameter param tgt
          Eval.SpecialLengthParameterVariableAssignment param tgt ->
            HSt.setSpecialLengthParameter param tgt
      Eval.ModifyVariable modCommand ->
        case modCommand of
          Eval.AdvanceIntVariable var plusVal ->
            case var of
              HSt.Var.ParamVar param ->
                HSt.advanceParameterValue param plusVal scope
              HSt.Var.RegisterVar registerLoc ->
                HSt.advanceRegisterValue registerLoc plusVal scope
          Eval.AdvanceLengthVariable var plusVal ->
            case var of
              HSt.Var.ParamVar param ->
                HSt.advanceParameterValue param plusVal scope
              HSt.Var.RegisterVar registerLoc ->
                HSt.advanceRegisterValue registerLoc plusVal scope
          Eval.AdvanceGlueVariable var plusVal ->
            case var of
              HSt.Var.ParamVar param ->
                HSt.advanceParameterValue param plusVal scope
              HSt.Var.RegisterVar registerLoc ->
                HSt.advanceRegisterValue registerLoc plusVal scope
          Eval.AdvanceMathGlueVariable var plusVal ->
            case var of
              HSt.Var.ParamVar param ->
                HSt.advanceParameterValue param plusVal scope
              HSt.Var.RegisterVar registerLoc ->
                HSt.advanceRegisterValue registerLoc plusVal scope
          Eval.ScaleVariable scaleDirection numVar scaleVal ->
            case numVar of
              Eval.IntNumericVariable var ->
                case var of
                  HSt.Var.ParamVar param ->
                    HSt.scaleParameterValue param scaleDirection scaleVal scope
                  HSt.Var.RegisterVar registerLoc ->
                    HSt.scaleRegisterValue registerLoc scaleDirection scaleVal scope
              Eval.LengthNumericVariable var ->
                case var of
                  HSt.Var.ParamVar param ->
                    HSt.scaleParameterValue param scaleDirection scaleVal scope
                  HSt.Var.RegisterVar registerLoc ->
                    HSt.scaleRegisterValue registerLoc scaleDirection scaleVal scope
              Eval.GlueNumericVariable var ->
                case var of
                  HSt.Var.ParamVar param ->
                    HSt.scaleParameterValue param scaleDirection scaleVal scope
                  HSt.Var.RegisterVar registerLoc ->
                    HSt.scaleRegisterValue registerLoc scaleDirection scaleVal scope
              Eval.MathGlueNumericVariable var ->
                case var of
                  HSt.Var.ParamVar param ->
                    HSt.scaleParameterValue param scaleDirection scaleVal scope
                  HSt.Var.RegisterVar registerLoc ->
                    HSt.scaleRegisterValue registerLoc scaleDirection scaleVal scope
      --   Eval.SelectFont fNr ->
      --     do
      --       selectFont fNr scope
      --       addVElem $ H.Inter.B.List.VListBaseElem $ H.Inter.B.Box.ElemFontSelection $ H.Inter.B.Box.FontSelection fNr
      --   Eval.SetFamilyMember fm fontRef ->
      --     do
      --       eFm <- texEvaluate fm
      --       fNr <- texEvaluate fontRef
      --       modifying' (typed @Config) $ setFamilyMemberFont eFm fNr scope
      --   -- Start a new level of grouping. Enter inner mode.
      --   Eval.SetBoxRegister lhsIdx box ->
      --     do
      --       eLhsIdx <- texEvaluate lhsIdx
      --       case box of
      --         Eval.FetchedRegisterBox fetchMode rhsIdx ->
      --           do
      --             fetchedMaybeBox <- fetchBox fetchMode rhsIdx
      --             modifying' (typed @Config) $ setBoxRegisterNullable eLhsIdx scope fetchedMaybeBox
      --         Eval.LastBox ->
      --           notImplemented "SetBoxRegister to LastBox"
      --         Eval.VSplitBox _ _ ->
      --           notImplemented "SetBoxRegister to VSplitBox"
      --         Eval.ExplicitBox spec boxType -> do
      --           eSpec <- texEvaluate spec
      --           modifying' (typed @Config) $ pushGroup (ScopeGroup newLocalScope ExplicitBoxGroup)
      --           extractedBox <- extractExplicitBox eSpec boxType
      --           modifying' (typed @Config) $ setBoxRegister eLhsIdx extractedBox scope
      --   Eval.SetFontChar (Eval.FontCharRef fontChar fontRef) charRef ->
      --     do
      --       fNr <- texEvaluate fontRef
      --       eCharRef <- texEvaluate charRef
      --       let updateFontChar f = case fontChar of
      --             Eval.SkewChar -> f {skewChar = eCharRef}
      --             Eval.HyphenChar -> f {hyphenChar = eCharRef}
      --       modifyFont fNr updateFontChar
      assignment ->
        notImplemented $ "Assignment body: " <> show assignment
    -- Now that we're done with an assignment, see whether any
    -- 'after-assignment' token was set.
    HSt.popAfterAssignmentToken >>= \case
      Nothing -> pure ()
      -- If a token was indeed set, put it into the input.
      Just lt -> lift $ Lex.insertLexTokenToSource lt
    pure DidNotSeeEndBox
  -- Start a new level of grouping.
  Eval.ChangeScope Q.Positive entryTrigger -> do
    HSt.pushGroup (Just (HSt.Group.LocalStructureScopeGroup entryTrigger))
    pure DidNotSeeEndBox
  -- -- Do the appropriate finishing actions, undo the
  -- -- effects of non-global assignments, and leave the
  -- -- group. Maybe leave the current mode.
  Eval.ChangeScope Q.Negative exitTrigger -> do
    --   prePopCurrentFontNr <- uses (typed @Config) lookupCurrentFontNr
    --   postPopCurrentFontNr <- uses (typed @Config) lookupCurrentFontNr
    --   when (prePopCurrentFontNr /= postPopCurrentFontNr) $ do
    --     addVElem $ H.Inter.B.List.VListBaseElem $ H.Inter.B.Box.ElemFontSelection $ H.Inter.B.Box.FontSelection (fromMaybe 0 postPopCurrentFontNr)
    HSt.popGroup exitTrigger
    pure DidNotSeeEndBox
  --   case group of
  --     -- Undo the effects of non-global
  --     -- assignments without leaving the
  --     -- current mode.
  --     ScopeGroup _ (LocalStructureScopeGroup entryTrig) -> do
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
  -- Eval.AddBox Eval.NaturalPlacement boxSource -> do
  --   case boxSource of
  --     Eval.FetchedRegisterBox fetchMode idx ->
  --       fetchBox fetchMode idx >>= \case
  --         Nothing ->
  --           pure ()
  --         Just b ->
  --           addVElem $ H.Inter.B.List.VListBaseElem $ H.Inter.B.Box.ElemBox b
  --     Eval.ExplicitBox spec boxType -> do
  --       -- Start a new level of grouping. Enter inner mode.
  --       eSpec <- texEvaluate spec
  --       modifying' (typed @Config) $ pushGroup (ScopeGroup newLocalScope ExplicitBoxGroup)
  --       b <- extractExplicitBox eSpec boxType
  --       addVElem $ H.Inter.B.List.VListBaseElem $ H.Inter.B.Box.ElemBox b
  --   pure DidNotSeeEndBox
  oth ->
    notImplemented $ "command " <> show oth
