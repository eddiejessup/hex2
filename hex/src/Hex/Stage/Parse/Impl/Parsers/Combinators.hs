module Hex.Stage.Parse.Impl.Parsers.Combinators where

import Control.Monad.Combinators qualified as PC
import Hex.Common.Ascii qualified as H.Ascii
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Resolve (ControlSymbol (..))
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken (PrimitiveToken)
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as T
import Hex.Common.Parse (MonadPrimTokenParse (..))
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hexlude

-- <optional spaces> = <zero or more spaces>.
skipOptionalSpaces :: MonadPrimTokenParse m => m ()
skipOptionalSpaces = skipManySatisfied isSpace

skipManySatisfied :: MonadPrimTokenParse m => (PrimitiveToken -> Bool) -> m ()
skipManySatisfied chk = PC.skipMany $ satisfyIf chk

skipOptional :: MonadPrimTokenParse m => (PrimitiveToken -> Bool) -> m ()
skipOptional = void . optional . satisfyIf

skipSatisfied :: MonadPrimTokenParse m => (PrimitiveToken -> Bool) -> m ()
skipSatisfied = void . satisfyIf

satisfyIf :: MonadPrimTokenParse m => (PrimitiveToken -> Bool) -> m PrimitiveToken
satisfyIf f = satisfyThen (\x -> if f x then Just x else Nothing)

isOnly :: forall k is s a. (Eq a, Is k An_AffineFold) => Optic' k is s a -> a -> s -> Bool
isOnly af x = is (castOptic @An_AffineFold af % castOptic @An_AffineFold (only x))

is :: Is k An_AffineFold => Optic' k is s a -> s -> Bool
is af s = not $ isn't af s

choiceFlap :: Alternative m => [t -> m a] -> t -> m a
choiceFlap headToParsers t =
  PC.choice (flap headToParsers t)

parseHeaded :: MonadPrimTokenParse m => (PrimitiveToken -> m a) -> m a
parseHeaded = (getAnyPrimitiveToken >>=)

satisfyLexThen :: MonadPrimTokenParse m => (Lex.LexToken -> Maybe a) -> m a
satisfyLexThen f = do
  lt <- getAnyLexToken
  maybe empty pure (f lt)

satisfyLexIf :: MonadPrimTokenParse m => (Lex.LexToken -> Bool) -> m Lex.LexToken
satisfyLexIf f = satisfyLexThen (\x -> if f x then Just x else Nothing)

skipSatisfiedLex :: MonadPrimTokenParse m => (Lex.LexToken -> Bool) -> m ()
skipSatisfiedLex = void . satisfyLexIf

primTokenHasCategory :: Code.CoreCatCode -> PrimitiveToken -> Bool
primTokenHasCategory = isOnly (T.primTokCharCat % typed @Code.CoreCatCode)

lexTokenHasCategory :: Code.CoreCatCode -> Lex.LexToken -> Bool
lexTokenHasCategory = isOnly Lex.lexTokCategory

primTokCatChar :: Code.CoreCatCode -> AffineFold PrimitiveToken Code.CharCode
primTokCatChar cat = T.primTokCharCat % filtered (isOnly (typed @Code.CoreCatCode) cat) % typed @Code.CharCode

-- More domainy.

skipOneOptionalSpace :: MonadPrimTokenParse m => m ()
skipOneOptionalSpace = skipOptional isSpace

-- <space token> = character token of category [space], or a control sequence
-- or active character \let equal to such.
isSpace :: PrimitiveToken -> Bool
isSpace = primTokenHasCategory Code.Space

matchNonActiveCharacterUncased :: Code.CharCode -> PrimitiveToken -> Bool
matchNonActiveCharacterUncased a pt =
  case pt ^? T.primTokCharCat of
    Just cc ->
      let aWord = a ^. typed @Word8
          chrWord = cc ^. typed @Code.CharCode % typed @Word8
       in (cc ^. typed @Code.CoreCatCode /= Code.Active) && (chrWord == H.Ascii.toUpper aWord || chrWord == H.Ascii.toLower aWord)
    _ ->
      False

skipKeyword :: MonadPrimTokenParse m => [Code.CharCode] -> m ()
skipKeyword s = do
  skipOptionalSpaces
  mapM_ (skipSatisfied . matchNonActiveCharacterUncased) s

parseOptionalKeyword :: MonadPrimTokenParse m => [Code.CharCode] -> m Bool
parseOptionalKeyword s = isJust <$> optional (skipKeyword s)

skipFiller :: MonadPrimTokenParse m => m ()
skipFiller = skipManySatisfied isFillerItem
  where
    isFillerItem :: PrimitiveToken -> Bool
    isFillerItem = \case
      T.RelaxTok -> True
      t -> primTokenHasCategory Code.Space t

skipOptionalEquals :: MonadPrimTokenParse m => m ()
skipOptionalEquals = do
  skipOptionalSpaces
  skipOptional $ isOnly (primTokCatChar Code.Other) (Code.Chr_ '=')

parseCSName :: forall m. MonadPrimTokenParse m => m ControlSymbol
parseCSName = satisfyLexThen lextokToCSLike
  where
    lextokToCSLike = \case
      Lex.CharCatLexToken (Lex.LexCharCat c Code.Active) ->
        Just $ ActiveCharacterSymbol c
      Lex.ControlSequenceLexToken cs ->
        Just $ ControlSequenceSymbol cs
      _ ->
        Nothing

parseXEqualsY :: MonadPrimTokenParse m => m a -> m b -> m (a, b)
parseXEqualsY parseX parseY = do
  x <- parseX
  skipOptionalEquals
  y <- parseY
  pure (x, y)
