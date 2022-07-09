module Hex.Stage.Parse.Impl.Parsers.Combinators where

import Control.Monad.Combinators qualified as PC
import Formatting qualified as F
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Ascii qualified as H.Ascii
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Resolve (ControlSymbol (..))
import Hex.Common.Parse.Interface (MonadPrimTokenParse (..))
import Hex.Common.Parse.Interface qualified as Par
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved.Primitive (PrimitiveToken)
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hexlude

data ExpansionMode = Expanding | Inhibited
  deriving stock (Show, Eq, Generic)

satisfyLexThen :: MonadPrimTokenParse m => ExpansionMode -> (LT.LexToken -> Maybe a) -> m a
satisfyLexThen mode f = case mode of
  Expanding -> satisfyThenExpanding (\(lt, _pt) -> f lt)
  Inhibited -> satisfyThenInhibited f

satisfyLexThenExpanding :: MonadPrimTokenParse m => (LT.LexToken -> Maybe a) -> m a
satisfyLexThenExpanding = satisfyLexThen Expanding

satisfyPrimThenExpanding :: MonadPrimTokenParse m => (PT.PrimitiveToken -> Maybe a) -> m a
satisfyPrimThenExpanding f = satisfyThenExpanding (\(_lt, pt) -> f pt)

type SatisfyThen m t a = (t -> Maybe a) -> m a

anyToken :: SatisfyThen m t t -> m t
anyToken satisfyThen = satisfyThen Just

anyLexExpanding :: MonadPrimTokenParse m => m LT.LexToken
anyLexExpanding = anyToken satisfyLexThenExpanding

anyLexInhibited :: MonadPrimTokenParse m => m LT.LexToken
anyLexInhibited = anyToken satisfyThenInhibited

anyPrim :: MonadPrimTokenParse m => m PrimitiveToken
anyPrim = anyToken satisfyPrimThenExpanding

satisfyIf :: SatisfyThen m t t -> (t -> Bool) -> m t
satisfyIf satisfyThen f = satisfyThen (\x -> if f x then Just x else Nothing)

skipManySatisfied :: MonadPlus m => SatisfyThen m t t -> (t -> Bool) -> m ()
skipManySatisfied satisfyThen chk = PC.skipMany $ satisfyIf satisfyThen chk

skipOptional :: MonadPlus m => SatisfyThen m t t -> (t -> Bool) -> m ()
skipOptional satisfyThen = void . optional . satisfyIf satisfyThen

skipSatisfied :: MonadPlus m => SatisfyThen m t t -> (t -> Bool) -> m ()
skipSatisfied satisfyThen = void . satisfyIf satisfyThen

satisfyEquals :: MonadPrimTokenParse m => PrimitiveToken -> m ()
satisfyEquals t = skipSatisfied satisfyPrimThenExpanding (== t)

satisfyLexEquals :: MonadPrimTokenParse m => ExpansionMode -> LT.LexToken -> m ()
satisfyLexEquals mode t = skipSatisfied (satisfyLexThen mode) (== t)

isOnly :: forall k is s a. (Eq a, Is k An_AffineFold) => Optic' k is s a -> a -> s -> Bool
isOnly af x = is (castOptic @An_AffineFold af % castOptic @An_AffineFold (only x))

is :: Is k An_AffineFold => Optic' k is s a -> s -> Bool
is af s = not $ isn't af s

choiceFlap :: MonadPlus m => [t -> m a] -> t -> m a
choiceFlap headToParsers t =
  PC.choice (flap headToParsers t)

-- <optional spaces> = <zero or more spaces>.
skipOptionalSpaces :: MonadPrimTokenParse m => ExpansionMode -> m ()
skipOptionalSpaces mode =
  skipManySatisfied (satisfyLexThen mode) lexTokenIsSpace

liftLexHead :: MonadPrimTokenParse m => (LT.LexToken -> m a) -> PrimitiveToken -> m a
liftLexHead lexParser pt =
  case pt ^? PT.primTokLexTok of
    Nothing -> Par.parseFailure $ "liftLexHead " <> F.sformat PT.fmtPrimitiveToken pt
    Just lt -> lexParser lt

primTokenHasCategory :: Code.CoreCatCode -> PrimitiveToken -> Bool
primTokenHasCategory = isOnly (PT.primTokCharCat % typed @Code.CoreCatCode)

lexTokenHasCategory :: Code.CoreCatCode -> LT.LexToken -> Bool
lexTokenHasCategory = isOnly LT.lexTokCategory

lexTokenCatChar :: Code.CoreCatCode -> AffineFold LT.LexToken Code.CharCode
lexTokenCatChar cat = LT.lexTokCharCat % filtered (isOnly (typed @Code.CoreCatCode) cat) % typed @Code.CharCode

skipOneOptionalSpace :: (MonadPrimTokenParse m) => ExpansionMode -> m ()
skipOneOptionalSpace mode = skipOptional (satisfyLexThen mode) lexTokenIsSpace

-- <space token> = character token of category [space], or a control sequence
-- or active character \let equal to such.
lexTokenIsSpace :: LT.LexToken -> Bool
lexTokenIsSpace = lexTokenHasCategory Code.Space

matchNonActiveCharacterUncased :: Code.CharCode -> LT.LexToken -> Bool
matchNonActiveCharacterUncased a t =
  case t ^? LT.lexTokCharCat of
    Just cc ->
      let aWord = a ^. typed @Word8
          chrWord = cc ^. typed @Code.CharCode % typed @Word8
       in (cc ^. typed @Code.CoreCatCode /= Code.Active) && (chrWord == H.Ascii.toUpper aWord || chrWord == H.Ascii.toLower aWord)
    _ ->
      False

skipKeyword :: MonadPrimTokenParse m => ExpansionMode -> [Code.CharCode] -> m ()
skipKeyword mode s = do
  skipOptionalSpaces mode
  for_ s (skipSatisfied (satisfyLexThen mode) . matchNonActiveCharacterUncased)

parseOptionalKeyword :: MonadPrimTokenParse m => ExpansionMode -> [Code.CharCode] -> m Bool
parseOptionalKeyword mode s =
  isJust <$> optional (skipKeyword mode s)

skipFillerExpanding :: MonadPrimTokenParse m => m ()
skipFillerExpanding = skipManySatisfied satisfyPrimThenExpanding isFillerItem
  where
    isFillerItem :: PrimitiveToken -> Bool
    isFillerItem = \case
      PT.RelaxTok -> True
      t -> primTokenHasCategory Code.Space t

skipOptionalEquals :: (MonadPrimTokenParse m) => ExpansionMode -> m ()
skipOptionalEquals mode = do
  skipOptionalSpaces mode
  skipOptional (satisfyLexThen mode) $ isOnly (lexTokenCatChar Code.Other) (Code.Chr_ '=')

parseControlSymbol :: MonadPrimTokenParse m => m ControlSymbol
parseControlSymbol = Par.satisfyThenInhibited lextokToCSLike
  where
    lextokToCSLike = \case
      LT.CharCatLexToken (LT.LexCharCat c Code.Active) ->
        Just $ ActiveCharacterSymbol c
      LT.ControlSequenceLexToken cs ->
        Just $ ControlSequenceSymbol cs
      _ ->
        Nothing

parseXEqualsY :: MonadPrimTokenParse m => ExpansionMode -> m a -> m b -> m (a, b)
parseXEqualsY mode parseX parseY = do
  x <- parseX
  Log.log "Successfully parsed X of X=Y"
  skipOptionalEquals mode
  Log.log "Successfully parsed equals-sign of X=Y"
  y <- parseY
  Log.log "Successfully parsed Y of X=Y"
  pure (x, y)
