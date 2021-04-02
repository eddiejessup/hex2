module Hex.Parse.Parsers.Combinators where

import Control.Monad.Combinators qualified as PC
import Hex.Codes qualified as H.C
import Hex.Lex.Types qualified as H.Lex
import Hex.Symbol.Tokens qualified as T
import Hexlude
import Hex.Symbol.Tokens (PrimitiveToken)
import Hex.Parse.MonadPrimTokenSource.Interface

-- <optional spaces> = <zero or more spaces>.
skipOptionalSpaces :: MonadPrimTokenSource m => m ()
skipOptionalSpaces = skipManySatisfied isSpace

skipManySatisfied :: MonadPrimTokenSource m => (PrimitiveToken -> Bool) -> m ()
skipManySatisfied chk = PC.skipMany $ satisfyIf chk

skipOneOptionalSpace :: MonadPrimTokenSource m => m ()
skipOneOptionalSpace = skipOptional isSpace

skipOptional :: MonadPrimTokenSource m => (PrimitiveToken -> Bool) -> m ()
skipOptional = void . optional . satisfyIf

skipSatisfied :: MonadPrimTokenSource m => (PrimitiveToken -> Bool) -> m ()
skipSatisfied = void . satisfyIf

satisfyIf :: MonadPrimTokenSource m => (PrimitiveToken -> Bool) -> m PrimitiveToken
satisfyIf f = satisfyThen (\x -> if f x then Just x else Nothing)

-- <space token> = character token of category [space], or a control sequence
-- or active character \let equal to such.
isSpace :: PrimitiveToken -> Bool
isSpace = isOnly (primTokCharCat % typed @H.C.CoreCatCode) H.C.Space

isOnly :: forall k is s a. (Eq a, Is k An_AffineFold) => Optic' k is s a -> a -> s -> Bool
isOnly af x = is (castOptic @An_AffineFold af % castOptic @An_AffineFold (only x))

is :: Is k An_AffineFold => Optic' k is s a -> s -> Bool
is af s = not $ isn't af s

primTokCharCat :: AffineTraversal' PrimitiveToken H.Lex.LexCharCat
primTokCharCat = castOptic @An_AffineTraversal (_Ctor @"UnresolvedTok") % H.Lex.lexTokCharCat

primTokCatChar :: H.C.CoreCatCode -> AffineFold PrimitiveToken H.C.CharCode
primTokCatChar cat = primTokCharCat % filtered (isOnly (typed @H.C.CoreCatCode) cat) % typed @H.C.CharCode

withInhibition :: MonadPrimTokenSource m => m a -> m a
withInhibition = panic "TODO: Inhibition"

fetchInhibitedLexToken :: MonadPrimTokenSource m => m H.Lex.LexToken
fetchInhibitedLexToken = do
  fetchPT >>= \case
    T.UnresolvedTok t -> pure t
    _ -> panic "impossible"

choiceFlap :: Alternative m => [t -> m a] -> t -> m a
choiceFlap headToParsers t =
  PC.choice (flap headToParsers t)

parseHeaded :: MonadPrimTokenSource m => (PrimitiveToken -> m a) -> m a
parseHeaded = (fetchPT >>=)
