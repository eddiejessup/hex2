module Hex.Stage.Parse.Impl.Parsers.Combinators where

import Control.Monad.Combinators qualified as PC
import Formatting qualified as F
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Ascii qualified as H.Ascii
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Resolve (ControlSymbol (..))
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved.Primitive (PrimitiveToken)
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hex.Stage.Expand.Interface (PrimTokenSource (..))
import Hex.Stage.Expand.Interface qualified as Par
import Hexlude

satisfyLexThen :: (PrimTokenSource :> es) => PT.ExpansionMode -> (LT.LexToken -> Maybe a) -> Eff es a
satisfyLexThen mode f = case mode of
  PT.Expanding -> Par.satisfyThenExpanding (\(lt, _pt) -> f lt)
  PT.Inhibited -> Par.satisfyThenInhibited f

satisfyCharCatThen :: (PrimTokenSource :> es) => PT.ExpansionMode -> (LT.LexCharCat -> Maybe a) -> Eff es a
satisfyCharCatThen mode f = satisfyLexThen mode $ \case
  LT.CharCatLexToken cc -> f cc
  _ -> Nothing

satisfyPrimThenExpanding :: (PrimTokenSource :> es) => (PT.PrimitiveToken -> Maybe a) -> Eff es a
satisfyPrimThenExpanding f = Par.satisfyThenExpanding (\(_lt, pt) -> f pt)

type SatisfyThen m t a = (t -> Maybe a) -> m a

anyToken :: SatisfyThen (Eff es) t t -> Eff es t
anyToken satisfyThen = satisfyThen Just

anyLexInMode :: (PrimTokenSource :> es) => PT.ExpansionMode -> Eff es LT.LexToken
anyLexInMode mode = anyToken (satisfyLexThen mode)

anyLexExpanding :: (PrimTokenSource :> es) => Eff es LT.LexToken
anyLexExpanding = anyLexInMode PT.Expanding

anyLexInhibited :: (PrimTokenSource :> es) => Eff es LT.LexToken
anyLexInhibited = anyLexInMode PT.Inhibited

anyPrim :: (PrimTokenSource :> es) => Eff es PrimitiveToken
anyPrim = anyToken satisfyPrimThenExpanding

satisfyIf :: SatisfyThen (Eff es) t t -> (t -> Bool) -> Eff es t
satisfyIf satisfyThen f = satisfyThen (\x -> if f x then Just x else Nothing)

skipManySatisfied :: MonadPlus (Eff es) => SatisfyThen (Eff es) t t -> (t -> Bool) -> Eff es ()
skipManySatisfied satisfyThen chk = PC.skipMany $ satisfyIf satisfyThen chk

skipOptional :: MonadPlus (Eff es) => SatisfyThen (Eff es) t t -> (t -> Bool) -> Eff es ()
skipOptional satisfyThen = void . optional . satisfyIf satisfyThen

skipSatisfied :: MonadPlus (Eff es) => SatisfyThen (Eff es) t t -> (t -> Bool) -> Eff es ()
skipSatisfied satisfyThen = void . satisfyIf satisfyThen

skipCharCatWithCategory ::
  (PrimTokenSource :> es, NonDet :> es) =>
  PT.ExpansionMode ->
  Code.CoreCatCode ->
  Eff es ()
skipCharCatWithCategory mode cat =
  skipSatisfied (satisfyCharCatThen mode) $ charCatHasCategory cat

satisfyPrimEquals :: (PrimTokenSource :> es, NonDet :> es) => PrimitiveToken -> Eff es ()
satisfyPrimEquals t = skipSatisfied satisfyPrimThenExpanding (== t)

satisfyLexEquals :: (PrimTokenSource :> es, NonDet :> es) => PT.ExpansionMode -> LT.LexToken -> Eff es ()
satisfyLexEquals mode t = skipSatisfied (satisfyLexThen mode) (== t)

isOnly :: forall k is s a. (Eq a, Is k An_AffineFold) => Optic' k is s a -> a -> s -> Bool
isOnly af x = is (castOptic @An_AffineFold af % castOptic @An_AffineFold (only x))

is :: Is k An_AffineFold => Optic' k is s a -> s -> Bool
is af s = not $ isn't af s

choiceFlap :: MonadPlus (Eff es) => [t -> Eff es a] -> t -> Eff es a
choiceFlap headToParsers t =
  PC.choice (flap headToParsers t)

-- <optional spaces> = <zero or more spaces>.
skipOptionalSpaces :: (PrimTokenSource :> es, NonDet :> es) => PT.ExpansionMode -> Eff es ()
skipOptionalSpaces mode =
  skipManySatisfied (satisfyCharCatThen mode) charCatIsSpace

liftLexHead :: (PrimTokenSource :> es) => (LT.LexCharCat -> Eff es a) -> PrimitiveToken -> Eff es a
liftLexHead lexParser pt =
  case pt ^? PT.primTokCharCat of
    Nothing -> Par.failParse $ Par.ParseExplicitFailure $ "liftLexHead " <> F.sformat PT.fmtPrimitiveToken pt
    Just lt -> lexParser lt

primTokenHasCategory :: Code.CoreCatCode -> PrimitiveToken -> Bool
primTokenHasCategory = isOnly (PT.primTokCharCat % typed @Code.CoreCatCode)

lexTokenHasCategory :: Code.CoreCatCode -> LT.LexToken -> Bool
lexTokenHasCategory = isOnly LT.lexTokCategory

charCatHasCategory :: Code.CoreCatCode -> LT.LexCharCat -> Bool
charCatHasCategory cat cc = cc.lexCCCat == cat

charCatChar :: Code.CoreCatCode -> AffineFold LT.LexCharCat Code.CharCode
charCatChar cat = filtered (isOnly (typed @Code.CoreCatCode) cat) % typed @Code.CharCode

skipOneOptionalSpace :: (PrimTokenSource :> es, NonDet :> es) => PT.ExpansionMode -> Eff es ()
skipOneOptionalSpace mode = skipOptional (satisfyCharCatThen mode) charCatIsSpace

skipSpace :: (PrimTokenSource :> es, NonDet :> es) => PT.ExpansionMode -> Eff es ()
skipSpace mode = skipCharCatWithCategory mode Code.Space

-- <space token> = character token of category [space], or a control sequence
-- or active character \let equal to such.
charCatIsSpace :: LT.LexCharCat -> Bool
charCatIsSpace = charCatHasCategory Code.Space

matchNonActiveCharacterUncased :: Code.CharCode -> LT.LexCharCat -> Bool
matchNonActiveCharacterUncased a cc =
  let aWord = a ^. typed @Word8
      chrWord = cc ^. typed @Code.CharCode % typed @Word8
   in (cc ^. typed @Code.CoreCatCode /= Code.Active) && (chrWord == H.Ascii.toUpper aWord || chrWord == H.Ascii.toLower aWord)

skipKeyword :: (PrimTokenSource :> es, NonDet :> es) => PT.ExpansionMode -> [Code.CharCode] -> Eff es ()
skipKeyword mode s = do
  skipOptionalSpaces mode
  for_ s (skipSatisfied (satisfyCharCatThen mode) . matchNonActiveCharacterUncased)

parseOptionalKeyword :: (PrimTokenSource :> es, NonDet :> es) => PT.ExpansionMode -> [Code.CharCode] -> Eff es Bool
parseOptionalKeyword mode s =
  isJust <$> optional (skipKeyword mode s)

skipFillerExpanding :: (PrimTokenSource :> es, NonDet :> es) => Eff es ()
skipFillerExpanding = skipManySatisfied satisfyPrimThenExpanding isFillerItem
  where
    isFillerItem :: PrimitiveToken -> Bool
    isFillerItem = \case
      PT.RelaxTok -> True
      t -> primTokenHasCategory Code.Space t

skipOptionalEquals :: (PrimTokenSource :> es, NonDet :> es) => PT.ExpansionMode -> Eff es ()
skipOptionalEquals mode = do
  skipOptionalSpaces mode
  skipOptional (satisfyCharCatThen mode) $ isOnly (ccCatChar Code.Other) (Code.Chr_ '=')

ccCatChar :: Code.CoreCatCode -> AffineFold LT.LexCharCat Code.CharCode
ccCatChar cat = filtered (isOnly (typed @Code.CoreCatCode) cat) % typed @Code.CharCode

parseControlSymbol :: (PrimTokenSource :> es) => Eff es ControlSymbol
parseControlSymbol = Par.satisfyThenInhibited lextokToCSLike
  where
    lextokToCSLike = \case
      LT.CharCatLexToken (LT.LexCharCat c Code.Active) ->
        Just $ ActiveCharacterSymbol c
      LT.ControlSequenceLexToken cs ->
        Just $ ControlSequenceSymbol cs
      _ ->
        Nothing

parseXEqualsY :: (PrimTokenSource :> es, NonDet :> es, Log.HexLog :> es) => PT.ExpansionMode -> Eff es a -> Eff es b -> Eff es (a, b)
parseXEqualsY mode parseX parseY = do
  x <- parseX
  Log.debugLog "Successfully parsed X of X=Y"
  skipOptionalEquals mode
  Log.debugLog "Successfully parsed equals-sign of X=Y"
  y <- parseY
  Log.debugLog "Successfully parsed Y of X=Y"
  pure (x, y)
