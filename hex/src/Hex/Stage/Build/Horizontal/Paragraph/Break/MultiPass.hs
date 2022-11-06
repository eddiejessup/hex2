module Hex.Stage.Build.Horizontal.Paragraph.Break.MultiPass where

import Data.Sequence qualified as Seq
import Formatting qualified as F
import GHC.Num qualified as Num
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Box qualified as Box
import Hex.Common.Codes qualified as Code
import Hex.Common.Font qualified as Font
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.AnyDirection.Breaking.Badness qualified as Bad
import Hex.Stage.Build.BoxElem qualified as BoxElem
import Hex.Stage.Build.Horizontal.Paragraph.Break.Common (LineBreakingEnv (..), mkLineBreakingEnv)
import Hex.Stage.Build.Horizontal.Paragraph.Break.Optimal qualified as Optimal
import Hex.Stage.Build.ListElem (HList)
import Hex.Stage.Build.ListElem qualified as ListElem
import Hexlude
import Hexlude.NonEmptySeq qualified as Seq.NE

data HyphenationState
  = SearchingForStartingLetter
  | SearchingForGlue
  | StartedWord MidWordState
  deriving stock (Generic)

data MidWordState = MidWordState
  { wordFont :: Font.FontNumber,
    hyphenItem :: ListElem.DiscretionaryItem,
    wordLetters :: NonEmptySeq (Box.Boxed Code.CharCode),
    suffixItems :: Seq ListElem.HListElem
  }
  deriving stock (Generic)

midWordLettersAsElems :: MidWordState -> Seq ListElem.HListElem
midWordLettersAsElems st = boxCharsAsElemSeq st.wordFont (Seq.NE.toSeq st.wordLetters)

data TrialWordEndOutcome
  = PassWordSuffix
  | HyphenateNow Bool -- seen glue?
  | AbandonHyphenation

midWordStateAccumulatedItems :: MidWordState -> Seq ListElem.HListElem
midWordStateAccumulatedItems st =
  midWordLettersAsElems st <> st.suffixItems

hyphStateAccumulatedItems :: HyphenationState -> Seq ListElem.HListElem
hyphStateAccumulatedItems = \case
  SearchingForStartingLetter -> Empty
  SearchingForGlue -> Empty
  StartedWord midWordState -> midWordStateAccumulatedItems midWordState

boxCharAsElem ::
  Font.FontNumber ->
  Box.Boxed Code.CharCode ->
  ListElem.HListElem
boxCharAsElem fNr boxChar =
  ListElem.HListHBaseElem $
    BoxElem.CharBoxHBaseElem $
      boxChar <&> \charCode ->
        BoxElem.CharBoxContents
          { charBoxCharCode = charCode,
            charBoxFont = fNr
          }

boxCharsAsElemSeq ::
  Functor f =>
  Font.FontNumber ->
  f (Box.Boxed Code.CharCode) ->
  f ListElem.HListElem
boxCharsAsElemSeq fNr = fmap (boxCharAsElem fNr)

hyphenateHList ::
  (HSt.EHexState :> es, Log.HexLog :> es) =>
  ListElem.HList ->
  Eff es ListElem.HList
hyphenateHList hList = do
  (hListElems, endState) <- runStateLocal SearchingForGlue (for hList.unHList handleHListElemInHyphenation)
  finalYieldedItems <- case endState of
    SearchingForGlue -> pure mempty
    SearchingForStartingLetter -> pure mempty
    StartedWord midWordState ->
      hyphenateWord midWordState

  let flattened = join (hListElems |> finalYieldedItems)
  pure $ ListElem.HList flattened

handleHListElemInHyphenation ::
  (HSt.EHexState :> es, State HyphenationState :> es, Log.HexLog :> es) =>
  ListElem.HListElem ->
  Eff es (Seq ListElem.HListElem)
handleHListElemInHyphenation x =
  get >>= \case
    SearchingForGlue -> do
      -- Tex looks for potentially hyphenatable words by searching ahead from
      -- each glue item that is not in a math formula.
      case x of
        ListElem.HVListElem vListElem ->
          put $ vListElemToNextState vListElem
        ListElem.DiscretionaryItemElem _ ->
          pure ()
        ListElem.HListHBaseElem _ ->
          pure ()
      pure $ Seq.singleton x
    SearchingForStartingLetter -> case x of
      -- The search bypasses:
      -- • Characters whose \lccode is zero
      -- • Ligatures that begin with such characters (TODO)
      -- • Whatsits (TODO)
      -- • Implicit kern items, i.e., kerns that were inserted by Tex itself
      --   because of information stored with the font (TODO)
      ListElem.HVListElem vListElem -> do
        put $ vListElemToNextState vListElem
        pure $ Seq.singleton x
      ListElem.DiscretionaryItemElem _ -> do
        accumItems <- abandonAndReset False
        pure $ accumItems |> x
      ListElem.HListHBaseElem hBaseElem -> case hBaseElem of
        BoxElem.CharBoxHBaseElem charBox -> do
          let boxCharCode = charBox.boxedContents.charBoxCharCode
          HSt.getHexCode Code.CLowerCaseCodeType boxCharCode >>= \case
            -- Bypass with zero lowercase code.
            Code.LowerCaseCode Code.NoCaseChange ->
              pure $ Seq.singleton x
            lcCode@(Code.LowerCaseCode _) -> do
              let continueAttempt = do
                    mayHyphenItem <- HSt.fontDiscretionaryHyphenItem charBox.boxedContents.charBoxFont
                    -- If a suitable starting letter is found, let it be in
                    -- font f.
                    -- Hyphenation is abandoned unless the \hyphenchar of f
                    -- is a number between 0 and 255, inclusive.
                    case mayHyphenItem of
                      Just hyphenItem -> do
                        put $
                          StartedWord
                            MidWordState
                              { wordFont = charBox.boxedContents.charBoxFont,
                                hyphenItem,
                                wordLetters = Seq.NE.singleton (charBox <&> (.charBoxCharCode)),
                                suffixItems = mempty
                              }
                        -- Don't emit the item yet, will do once we've handled hyphenation for this word.
                        pure Seq.Empty
                      Nothing -> do
                        accumItems <- abandonAndReset False
                        pure $ accumItems |> x
              -- If the starting letter is not lowercase, i.e, if it
              -- doesn’t equal its own \lccode, hyphenation is
              -- abandoned unless \uchyph is positive.
              if Code.codeIsLowerCaseLetter boxCharCode lcCode
                then continueAttempt
                else do
                  ucHyph <- HSt.getParameterValue (HSt.Param.IntQuantParam HSt.Param.UCHyph)
                  Log.debugLog $ "\\ucHyph = " <> F.sformat Q.fmtHexIntSimple ucHyph
                  if ucHyph > Q.zeroInt
                    then continueAttempt
                    else do
                      accumItems <- abandonAndReset False
                      pure $ accumItems |> x
    StartedWord midWordState -> do
      outcomeOrNewLetters <- case x of
        ListElem.HVListElem vListElem ->
          pure $
            Left $ case vListElem of
              ListElem.VListBaseElem baseElem -> case baseElem of
                BoxElem.AxOrRuleBoxBaseElem _box ->
                  AbandonHyphenation
                BoxElem.KernBaseElem _kern ->
                  HyphenateNow False
              ListElem.ListGlue _glue ->
                HyphenateNow True
              ListElem.ListPenalty _penalty ->
                HyphenateNow False
        ListElem.DiscretionaryItemElem _ ->
          pure $ Left AbandonHyphenation
        ListElem.HListHBaseElem hBaseElem -> case hBaseElem of
          BoxElem.CharBoxHBaseElem charBox -> case midWordState.suffixItems of
            -- Scan forward until coming to something that’s not one of the
            -- following “admissible items”:
            -- (1) A character in font f whose \lccode is nonzero
            -- (2) a ligature formed entirely from characters of type (1)
            -- (3) an implicit kern
            -- The first inadmissible item terminates this part of the process.
            -- the trial word consists of all the letters found in admissible
            -- items.
            -- Note that all of these letters are in font f.
            Empty ->
              -- Following the trial word can be zero or more of:
              -- • Character from another font
              -- • Character with zero \lccode
              -- • Ligature
              -- • Implicit kern
              if charBox.boxedContents.charBoxFont /= midWordState.wordFont
                then pure $ Left PassWordSuffix
                else
                  HSt.getHexCode Code.CLowerCaseCodeType (charBox.boxedContents.charBoxCharCode) <&> \case
                    Code.LowerCaseCode Code.NoCaseChange ->
                      Left PassWordSuffix
                    Code.LowerCaseCode _ ->
                      Right $ Seq.NE.singleton (charBox <&> (.charBoxCharCode))
            -- After these items, if any, must follow:
            -- • Glue
            -- • Explicit kern
            -- • Penalty
            -- • Whatsit
            -- • A \mark, \insert, or \vadjust item
            _ ->
              pure $ Left PassWordSuffix
      case outcomeOrNewLetters of
        Left outcome -> case outcome of
          -- If we saw something that indicates we entered or stayed in a post-word
          -- suffix,
          -- then move/stay in that state, appending the new suffix item.
          PassWordSuffix -> do
            modifying @HyphenationState (_Typed @MidWordState % #suffixItems) (|> x)
            pure Empty
          -- If we saw something that indicates we should hyphenate now,
          -- apply one last check:
          --   hyphenation will still be abandoned unless n ≥ λ + ρ, where λ = max(1,
          --   \lefthyphenmin) and ρ = max(1, \righthyphenmin)
          -- Otherwise,
          -- • Do the hyphenation,
          -- • Move to the relevant searching-state, depending on whether we just
          --   saw glue or something else.
          -- • Produce the hyphenated items as elems, plus the current item
          HyphenateNow seenGlue -> do
            yieldedItems <- hyphenateWord midWordState
            reset seenGlue
            pure $ yieldedItems |> x
          -- If we saw something that indicates we actually shouldn't hyphenate this word,
          -- get the items we accumulated for the word and suffix (if present),
          -- return those and the current item, and move to the relevant search-state.
          AbandonHyphenation -> do
            accumItems <- abandonAndReset False
            pure $ accumItems |> x
        Right admissableLetters -> do
          modifying @HyphenationState (_Typed @MidWordState % #wordLetters) (<> admissableLetters)
          -- Don't emit the item yet, will do once we've handled hyphenation for this word.
          pure Empty
  where
    vListElemToNextState = \case
      ListElem.VListBaseElem baseElem -> case baseElem of
        BoxElem.AxOrRuleBoxBaseElem _box ->
          SearchingForGlue
        BoxElem.KernBaseElem _kern ->
          SearchingForGlue
      ListElem.ListGlue _glue ->
        SearchingForStartingLetter
      ListElem.ListPenalty _penalty ->
        SearchingForGlue

hyphenateWord ::
  (HSt.EHexState :> es, Log.HexLog :> es) =>
  MidWordState ->
  Eff es (Seq ListElem.HListElem)
hyphenateWord midWordState = do
  leftHyphenMin <- HSt.getParameterValue (HSt.Param.IntQuantParam HSt.Param.LeftHyphenMin)
  rightHyphenMin <- HSt.getParameterValue (HSt.Param.IntQuantParam HSt.Param.RightHyphenMin)
  let hyphenMin = leftHyphenMin <> rightHyphenMin
  let n = Seq.NE.length midWordState.wordLetters

  if n < hyphenMin.unHexInt
    then pure $ midWordStateAccumulatedItems midWordState
    else do
      Log.infoLog $ "Hyphenation: Found word: " <> (Code.codesAsText $ toList $ midWordState.wordLetters <&> (.boxedContents))
      let -- If leftHyphenMin is zero, then all indexes are valid.
          -- If leftHyphenMin is one, then index must be >= 1.
          -- etc.
          minIx = leftHyphenMin.unHexInt
          -- Maximum index is n-1
          -- But really maximum we can hyphenate after is n-2, because we
          -- can't insert a hyphen after the last letter.
          -- If rightHyphenMin is zero, then all indexes are valid. So max is n-1
          -- If rightHyphenMin is one, then max valid is n-2
          -- This might result in a negative value, which will never be valid.
          -- This is fine, it just means we can't hyphenate any characters
          maxIx = min (n Num.- 2) (pred (n Num.- rightHyphenMin.unHexInt))
          hyphenationWordBoxChars =
            join $
              indexed (toList midWordState.wordLetters) <&> \(i, v) ->
                if i >= minIx && i <= maxIx
                  then [boxCharAsElem midWordState.wordFont v, ListElem.DiscretionaryItemElem midWordState.hyphenItem]
                  else [boxCharAsElem midWordState.wordFont v]
      pure $ Seq.fromList hyphenationWordBoxChars <> midWordState.suffixItems

abandonAndReset ::
  State HyphenationState :> es =>
  Bool ->
  Eff es (Seq ListElem.HListElem)
abandonAndReset seenGlue = do
  accumItems <- gets hyphStateAccumulatedItems
  reset seenGlue
  pure accumItems

reset :: State HyphenationState :> es => Bool -> Eff es ()
reset seenGlue =
  put $ if seenGlue then SearchingForStartingLetter else SearchingForGlue

breakHListMultiPass ::
  (Log.HexLog :> es, HSt.EHexState :> es) =>
  ListElem.HList ->
  Eff es (Seq HList)
breakHListMultiPass rawHList = do
  hSize <- HSt.getParameterValue (HSt.Param.LengthQuantParam HSt.Param.HSize)
  preTolerance <- HSt.getParameterValue (HSt.Param.IntQuantParam HSt.Param.PreTolerance)
  tolerance <- HSt.getParameterValue (HSt.Param.IntQuantParam HSt.Param.Tolerance)
  linePenalty <- HSt.getParameterValue (HSt.Param.IntQuantParam HSt.Param.LinePenalty)
  emergencyStretch <- HSt.getParameterValue (HSt.Param.LengthQuantParam HSt.Param.EmergencyStretch)
  hyphenPenalty <- HSt.getParameterValue (HSt.Param.IntQuantParam HSt.Param.HyphenPenalty)
  exHyphenPenalty <- HSt.getParameterValue (HSt.Param.IntQuantParam HSt.Param.ExHyphenPenalty)
  leftSkip <- HSt.getParameterValue (HSt.Param.GlueQuantParam HSt.Param.LeftSkip)
  rightSkip <- HSt.getParameterValue (HSt.Param.GlueQuantParam HSt.Param.RightSkip)

  let breakingEnv =
        mkLineBreakingEnv
          hSize
          emergencyStretch
          hyphenPenalty
          exHyphenPenalty
          preTolerance
          linePenalty
          leftSkip
          rightSkip

  passOneResult <- runReader breakingEnv $ do
    activeTol <- know @LineBreakingEnv #tolerance
    if (activeTol >= Bad.zeroFiniteBadness)
      then breakIt rawHList
      else pure Nothing
  case passOneResult of
    Just hLists -> pure hLists
    Nothing -> do
      Log.infoLog $
        F.sformat
          ( "Failed to break with pretolerance: "
              |%| Q.fmtHexIntSimple
              |%| ", hyphenating and retrying with tolerance: "
              |%| Q.fmtHexIntSimple
          )
          preTolerance
          tolerance
      hListWithDiscretionaries <- hyphenateHList rawHList
      let secondPassEnv =
            mkLineBreakingEnv
              hSize
              emergencyStretch
              hyphenPenalty
              exHyphenPenalty
              tolerance
              linePenalty
              leftSkip
              rightSkip

      runReader secondPassEnv (breakIt hListWithDiscretionaries) >>= \case
        Just hLists -> pure hLists
        Nothing ->
          if emergencyStretch > Q.zeroLength
            then notImplemented "Emergency stretch"
            else notImplemented "breakHListMultiPass: Hyphenated pass fails"
  where
    breakIt hList =
      Optimal.breakHListOptimally hList
