module Hex.Stage.Parse.Impl.Parsers.Combinators where

import Control.Monad.Combinators qualified as PC
import Hex.Common.Ascii qualified as H.Ascii
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Resolve (ControlSymbol (..))
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken (PrimitiveToken)
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.Parse.Interface (MonadPrimTokenParse (..))
import Hex.Common.Parse.Interface qualified as Par
import Hex.Stage.Lex.Interface.Extract (LexToken)
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hexlude

-- data ExpansionMode = Expanding | NotExpanding

satisfyThen :: (Monad m, Alternative m) => m t -> (t -> Maybe a) -> m a
satisfyThen getTok f = do
  t <- getTok
  maybe empty pure (f t)

satisfyIf :: (Monad m, Alternative m) => m t -> (t -> Bool) -> m t
satisfyIf getTok f = satisfyThen getTok (\x -> if f x then Just x else Nothing)

skipManySatisfied :: MonadPlus m => m t -> (t -> Bool) -> m ()
skipManySatisfied getTok chk = PC.skipMany $ satisfyIf getTok chk

skipOptional :: MonadPlus m => m t -> (t -> Bool) -> m ()
skipOptional getTok = void . optional . satisfyIf getTok

skipSatisfied :: MonadPlus m => m t -> (t -> Bool) -> m ()
skipSatisfied getTok = void . satisfyIf getTok

satisfyEquals :: MonadPrimTokenParse m => PrimitiveToken -> m ()
satisfyEquals t = skipSatisfied Par.getExpandedPrimitiveToken (== t)

satisfyLexEquals :: MonadPrimTokenParse m => ExpansionMode -> Lex.LexToken -> m ()
satisfyLexEquals mode t = skipSatisfied (getLexTokenInMode mode) (== t)

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
  skipManySatisfied (getLexTokenInMode mode) lexTokenIsSpace

liftLexHead :: MonadPlus m => (LexToken -> m a) -> PrimitiveToken -> m a
liftLexHead lexParser pt =
  case pt ^? PT.primTokLexTok of
    Nothing -> empty
    Just lt -> lexParser lt

primTokenHasCategory :: Code.CoreCatCode -> PrimitiveToken -> Bool
primTokenHasCategory = isOnly (PT.primTokCharCat % typed @Code.CoreCatCode)

lexTokenHasCategory :: Code.CoreCatCode -> LexToken -> Bool
lexTokenHasCategory = isOnly Lex.lexTokCategory

lexTokenCatChar :: Code.CoreCatCode -> AffineFold LexToken Code.CharCode
lexTokenCatChar cat = Lex.lexTokCharCat % filtered (isOnly (typed @Code.CoreCatCode) cat) % typed @Code.CharCode

skipOneOptionalSpace :: (MonadPrimTokenParse m) => ExpansionMode -> m ()
skipOneOptionalSpace mode = skipOptional (getLexTokenInMode mode) lexTokenIsSpace

-- <space token> = character token of category [space], or a control sequence
-- or active character \let equal to such.
lexTokenIsSpace :: LexToken -> Bool
lexTokenIsSpace = lexTokenHasCategory Code.Space

matchNonActiveCharacterUncased :: Code.CharCode -> LexToken -> Bool
matchNonActiveCharacterUncased a t =
  case t ^? Lex.lexTokCharCat of
    Just cc ->
      let aWord = a ^. typed @Word8
          chrWord = cc ^. typed @Code.CharCode % typed @Word8
       in (cc ^. typed @Code.CoreCatCode /= Code.Active) && (chrWord == H.Ascii.toUpper aWord || chrWord == H.Ascii.toLower aWord)
    _ ->
      False

skipKeyword :: MonadPrimTokenParse m => ExpansionMode -> [Code.CharCode] -> m ()
skipKeyword mode s = do
  skipOptionalSpaces mode
  mapM_ (skipSatisfied (getLexTokenInMode mode) . matchNonActiveCharacterUncased) s

parseOptionalKeyword :: MonadPrimTokenParse m => ExpansionMode -> [Code.CharCode] -> m Bool
parseOptionalKeyword mode s =
  isJust <$> optional (skipKeyword mode s)

skipFillerExpanding :: MonadPrimTokenParse m => m ()
skipFillerExpanding = skipManySatisfied Par.getExpandedPrimitiveToken isFillerItem
  where
    isFillerItem :: PrimitiveToken -> Bool
    isFillerItem = \case
      PT.RelaxTok -> True
      t -> primTokenHasCategory Code.Space t

skipOptionalEquals :: (MonadPrimTokenParse m) => ExpansionMode -> m ()
skipOptionalEquals mode = do
  skipOptionalSpaces mode
  skipOptional (getLexTokenInMode mode) $ isOnly (lexTokenCatChar Code.Other) (Code.Chr_ '=')

parseCSName :: MonadPrimTokenParse m => m ControlSymbol
parseCSName = satisfyThen getUnexpandedToken lextokToCSLike
  where
    lextokToCSLike = \case
      Lex.CharCatLexToken (Lex.LexCharCat c Code.Active) ->
        Just $ ActiveCharacterSymbol c
      Lex.ControlSequenceLexToken cs ->
        Just $ ControlSequenceSymbol cs
      _ ->
        Nothing

data ExpansionMode = Expanding | NotExpanding

getLexTokenInMode :: MonadPrimTokenParse m => ExpansionMode -> m LexToken
getLexTokenInMode = \case
  Expanding -> Par.getExpandedLexToken
  NotExpanding -> getUnexpandedToken

parseXEqualsY :: MonadPrimTokenParse m => ExpansionMode -> m a -> m b -> m (a, b)
parseXEqualsY mode parseX parseY = do
  x <- parseX
  skipOptionalEquals mode
  y <- parseY
  pure (x, y)
