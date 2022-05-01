module Hex.Stage.Expand.Interface where

import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as H.Sym.Tok
import Hexlude

data ParsingError
  = ParseFailure
  | SawUnexpectedToken UnexpectedToken
  deriving stock (Show, Eq, Generic)

data UnexpectedToken = UnexpectedToken {saw :: H.Sym.Tok.PrimitiveToken, expected :: Text}
  deriving stock (Show, Eq, Generic)

class (MonadPlus m, Monad m) => MonadPrimTokenSource m where
  fetchPT :: m H.Sym.Tok.PrimitiveToken

  satisfyThen :: (H.Sym.Tok.PrimitiveToken -> Maybe a) -> m a

  parseError :: ParsingError -> m a
