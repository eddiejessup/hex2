module Hex.Stage.Interpret.CommandHandler.AllMode where

import Formatting qualified as F
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Resolve qualified as Res
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Stage.Evaluate.Interface qualified as Eval
import Hex.Stage.Evaluate.Interface.AST.Command qualified as Eval
import Hex.Stage.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Stage.Interpret.Build.List.Elem qualified as H.Inter.B.List
import Hex.Stage.Lex.Interface qualified as Lex
import Hex.Stage.Parse.Interface qualified as Par
import Hexlude
import qualified Hex.Common.HexState.Interface.Resolve.SyntaxToken as ST
import qualified Hex.Stage.Lex.Interface.Extract as Lex

data InterpretError
  = SawEndBoxInMainVMode
  | SawEndBoxInMainVModePara -- "No box to end: in paragraph within main V mode"
  | NoFontSelected
  | UnexpectedEndOfInput
  deriving stock (Generic, Show)

fmtInterpretError :: Fmt InterpretError
fmtInterpretError = F.shown

data AllModeCommandResult
  = SawEndBox
  | DidNotSeeEndBox

getNextCommandLogged ::
  ( Eval.MonadEvaluate m,
    Par.MonadCommandSource m,
    MonadError e m,
    AsType InterpretError e,
    Log.MonadHexLog m
  ) =>
  m Eval.Command
getNextCommandLogged = do
  cmd <- Eval.getEvalCommandErrorEOF $ injectTyped UnexpectedEndOfInput
  Log.logText $ F.sformat ("Read command: " |%| F.shown) cmd
  pure cmd

handleModeIndependentCommand ::
  ( Monad m,
    MonadIO m,
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
    let _handle = case stdStream of
          PT.StdOut -> stdout
          PT.StdErr -> stderr
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
              Lex.ControlSequenceLexToken controlSequence -> do
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
            notImplemented ""
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
            HSt.setHexCode idxChar catCode scope
          Eval.MathCodeValue mathCode ->
            HSt.setHexCode idxChar mathCode scope
          Eval.UpperCaseCodeValue upperCaseCode ->
            HSt.setHexCode idxChar upperCaseCode scope
          Eval.LowerCaseCodeValue lowerCaseCode ->
            HSt.setHexCode idxChar lowerCaseCode scope
          Eval.SpaceFactorCodeValue spaceFactorCode ->
            HSt.setHexCode idxChar spaceFactorCode scope
          Eval.DelimiterCodeValue delimiterCode ->
            HSt.setHexCode idxChar delimiterCode scope
      Eval.SetVariable ass ->
        case ass of
          Eval.IntVariableAssignment (Eval.QuantVariableAssignment v tgt) ->
            case v of
              Eval.ParamVar intParam ->
                HSt.setScopedParameterValue intParam tgt scope
              Eval.RegisterVar registerLoc ->
                HSt.setScopedRegisterValue registerLoc tgt scope
          Eval.LengthVariableAssignment (Eval.QuantVariableAssignment v tgt) ->
            case v of
              Eval.ParamVar lengthParam ->
                HSt.setScopedParameterValue lengthParam tgt scope
              Eval.RegisterVar registerLoc ->
                HSt.setScopedRegisterValue registerLoc tgt scope
          Eval.GlueVariableAssignment (Eval.QuantVariableAssignment v tgt) ->
            case v of
              Eval.ParamVar glueParam ->
                HSt.setScopedParameterValue glueParam tgt scope
              Eval.RegisterVar registerLoc ->
                HSt.setScopedRegisterValue registerLoc tgt scope
          Eval.MathGlueVariableAssignment (Eval.QuantVariableAssignment v _tgt) ->
            case v of
              Eval.ParamVar _mathGlueParam ->
                notImplemented "MathGlueVariableAssignment, parameter-variable"
              Eval.RegisterVar _registerLoc ->
                notImplemented "MathGlueVariableAssignment, register-variable"
          Eval.TokenListVariableAssignment (Eval.QuantVariableAssignment v _tgt) ->
            case v of
              Eval.ParamVar _tokenListParam ->
                notImplemented "TokenListVariableAssignment, parameter-variable"
              Eval.RegisterVar _registerLoc ->
                notImplemented "TokenListVariableAssignment, register-variable"
          Eval.SpecialIntParameterVariableAssignment param tgt ->
            HSt.setSpecialIntParameter param tgt
          Eval.SpecialLengthParameterVariableAssignment param tgt ->
            HSt.setSpecialLengthParameter param tgt
      --   Eval.ModifyVariable modCommand ->
      --     case modCommand of
      --       Eval.AdvanceIntVariable var plusVal ->
      --         Var.advanceValueFromAST var scope plusVal
      --       Eval.AdvanceLengthVariable var plusVal ->
      --         Var.advanceValueFromAST var scope plusVal
      --       Eval.AdvanceGlueVariable var plusVal ->
      --         Var.advanceValueFromAST var scope plusVal
      --       Eval.AdvanceMathGlueVariable var plusVal ->
      --         Var.advanceValueFromAST var scope plusVal
      --       Eval.ScaleVariable vDir numVar scaleVal ->
      --         case numVar of
      --           Eval.HexIntNumericVariable var ->
      --             Var.scaleValueFromAST var scope vDir scaleVal
      --           Eval.LengthNumericVariable var ->
      --             Var.scaleValueFromAST var scope vDir scaleVal
      --           Eval.GlueNumericVariable var ->
      --             Var.scaleValueFromAST var scope vDir scaleVal
      --           Eval.MathGlueNumericVariable var ->
      --             Var.scaleValueFromAST var scope vDir scaleVal
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
  -- Eval.WriteToStream n (Eval.ImmediateWriteText eTxt) -> do
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
  -- Eval.ChangeScope Eval.Positive entryTrig ->
  --   do
  --     modifying' (typed @Config) $ pushGroup (ScopeGroup newLocalScope (LocalStructureGroup entryTrig))
  --     pure DidNotSeeEndBox
  -- -- Do the appropriate finishing actions, undo the
  -- -- effects of non-global assignments, and leave the
  -- -- group. Maybe leave the current mode.
  -- Eval.ChangeScope Eval.Negative exitTrig -> do
  --   prePopCurrentFontNr <- uses (typed @Config) lookupCurrentFontNr
  --   (group, poppedConfig) <- uses (typed @Config) popGroup >>= \case
  --     Nothing ->
  --       throwError $ injectTyped $ ConfigError "No group to leave"
  --     Just v ->
  --       pure v
  --   assign' (typed @Config) poppedConfig
  --   postPopCurrentFontNr <- uses (typed @Config) lookupCurrentFontNr
  --   when (prePopCurrentFontNr /= postPopCurrentFontNr) $ do
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
