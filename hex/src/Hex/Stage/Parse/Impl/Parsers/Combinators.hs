module Hex.Stage.Parse.Impl.Parsers.Combinators where

import Control.Monad.Combinators qualified as PC
import Hex.Common.Ascii qualified as H.Ascii
import Hex.Common.Codes qualified as H.C
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken (PrimitiveToken)
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as T
import Hexlude
import Hex.Stage.Expand.Interface (MonadPrimTokenSource (..))
import qualified Hex.Stage.Lex.Interface.Extract as Lex
import Hex.Common.HexState.Interface.Resolve (ControlSymbol (..))

-- <optional spaces> = <zero or more spaces>.
skipOptionalSpaces :: MonadPrimTokenSource m => m ()
skipOptionalSpaces = skipManySatisfied isSpace

skipManySatisfied :: MonadPrimTokenSource m => (PrimitiveToken -> Bool) -> m ()
skipManySatisfied chk = PC.skipMany $ satisfyIf chk

skipOptional :: MonadPrimTokenSource m => (PrimitiveToken -> Bool) -> m ()
skipOptional = void . optional . satisfyIf

skipSatisfied :: MonadPrimTokenSource m => (PrimitiveToken -> Bool) -> m ()
skipSatisfied = void . satisfyIf

satisfyIf :: MonadPrimTokenSource m => (PrimitiveToken -> Bool) -> m PrimitiveToken
satisfyIf f = satisfyThen (\x -> if f x then Just x else Nothing)

isOnly :: forall k is s a. (Eq a, Is k An_AffineFold) => Optic' k is s a -> a -> s -> Bool
isOnly af x = is (castOptic @An_AffineFold af % castOptic @An_AffineFold (only x))

is :: Is k An_AffineFold => Optic' k is s a -> s -> Bool
is af s = not $ isn't af s

data InhibitionToken = InhibitionToken

-- TODO: Implement me.
withInhibition :: (InhibitionToken -> m a) -> m a
withInhibition k = k InhibitionToken

choiceFlap :: Alternative m => [t -> m a] -> t -> m a
choiceFlap headToParsers t =
  PC.choice (flap headToParsers t)

parseHeaded :: MonadPrimTokenSource m => (PrimitiveToken -> m a) -> m a
parseHeaded = (fetchPT >>=)

-- Bit more domainy.

inhibSatisfyLexThen :: MonadPrimTokenSource m => InhibitionToken -> (Lex.LexToken -> Maybe a) -> m a
inhibSatisfyLexThen inhibToken f = do
  lt <- inhibFetchLexToken inhibToken
  maybe empty pure (f lt)

primTokHasCategory :: H.C.CoreCatCode -> PrimitiveToken -> Bool
primTokHasCategory = isOnly (primTokCharCat % typed @H.C.CoreCatCode)

primTokLexTok :: AffineTraversal' PrimitiveToken Lex.LexToken
primTokLexTok = castOptic @An_AffineTraversal (_Ctor @"UnresolvedTok")

primTokCharCat :: AffineTraversal' PrimitiveToken Lex.LexCharCat
primTokCharCat = primTokLexTok % Lex.lexTokCharCat

primTokCatChar :: H.C.CoreCatCode -> AffineFold PrimitiveToken H.C.CharCode
primTokCatChar cat = primTokCharCat % filtered (isOnly (typed @H.C.CoreCatCode) cat) % typed @H.C.CharCode

-- More domainy.

skipOneOptionalSpace :: MonadPrimTokenSource m => m ()
skipOneOptionalSpace = skipOptional isSpace

-- <space token> = character token of category [space], or a control sequence
-- or active character \let equal to such.
isSpace :: PrimitiveToken -> Bool
isSpace = primTokHasCategory H.C.Space

inhibFetchLexToken :: MonadPrimTokenSource m => InhibitionToken -> m Lex.LexToken
inhibFetchLexToken _ = do
  fetchPT >>= \case
    T.UnresolvedTok t -> pure t
    _ -> panic "impossible"

fetchInhibitedLexToken :: MonadPrimTokenSource m => m Lex.LexToken
fetchInhibitedLexToken = withInhibition inhibFetchLexToken

matchNonActiveCharacterUncased :: H.C.CharCode -> PrimitiveToken -> Bool
matchNonActiveCharacterUncased a pt =
  case pt ^? primTokCharCat of
    Just cc ->
      let aWord = a ^. typed @Word8
          chrWord = cc ^. typed @H.C.CharCode % typed @Word8
       in (cc ^. typed @H.C.CoreCatCode /= H.C.Active) && (chrWord == H.Ascii.toUpper aWord || chrWord == H.Ascii.toLower aWord)
    _ ->
      False

skipKeyword :: MonadPrimTokenSource m => [H.C.CharCode] -> m ()
skipKeyword s = do
  skipOptionalSpaces
  mapM_ (skipSatisfied . matchNonActiveCharacterUncased) s

parseOptionalKeyword :: MonadPrimTokenSource m => [H.C.CharCode] -> m Bool
parseOptionalKeyword s = isJust <$> optional (skipKeyword s)

skipFiller :: MonadPrimTokenSource m => m ()
skipFiller = skipManySatisfied isFillerItem
  where
    isFillerItem :: PrimitiveToken -> Bool
    isFillerItem = \case
      T.RelaxTok -> True
      t -> primTokHasCategory H.C.Space t

skipOptionalEquals :: MonadPrimTokenSource m => m ()
skipOptionalEquals = do
  skipOptionalSpaces
  skipOptional $ isOnly (primTokCatChar H.C.Other) (H.C.Chr_ '=')

parseCSName :: forall m. MonadPrimTokenSource m => m ControlSymbol
parseCSName = withInhibition unsafeParseCSName
  where
    unsafeParseCSName :: InhibitionToken -> m ControlSymbol
    unsafeParseCSName inhibToken = inhibSatisfyLexThen inhibToken lextokToCSLike
      where
        lextokToCSLike = \case
          Lex.CharCatLexToken (Lex.LexCharCat c H.C.Active) ->
            Just $ ActiveCharacterSymbol c
          Lex.ControlSequenceLexToken cs ->
            Just $ ControlSequenceSymbol cs
          _ ->
            Nothing

parseXEqualsY :: MonadPrimTokenSource m => m a -> m b -> m (a, b)
parseXEqualsY parseX parseY = do
  x <- parseX
  skipOptionalEquals
  y <- parseY
  pure (x, y)
