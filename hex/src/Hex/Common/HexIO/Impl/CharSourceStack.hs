module Hex.Common.HexIO.Impl.CharSourceStack where

import Data.List.NonEmpty qualified as L.NE
import Hex.Common.Codes qualified as Code
import Hex.Common.HexIO.Impl.CharSource qualified as CharSource
import Hexlude

-- The input to Tex is a sequence of “lines.”
newtype CharSourceStack = CharSourceStack {unCharSourceStack :: NonEmpty CharSource.CharSource}
  deriving stock (Show, Generic)

newCharSourceStack :: Text -> Maybe Code.CharCode -> ByteString -> CharSourceStack
newCharSourceStack name mayEndLineChar sourceBytes =
  CharSourceStack (L.NE.singleton (CharSource.newCharSource name mayEndLineChar sourceBytes))

pushCharSource :: Text -> Maybe Code.CharCode -> ByteString -> CharSourceStack -> CharSourceStack
pushCharSource name mayEndLineChar sourceBytes stack =
  let newSource = CharSource.newCharSource name mayEndLineChar sourceBytes
   in stack & #unCharSourceStack %~ L.NE.cons newSource

currentSourceLens :: Lens' CharSourceStack CharSource.CharSource
currentSourceLens = #unCharSourceStack % nonEmptyHeadLens

nonEmptyHeadLens :: Lens' (NonEmpty a) a
nonEmptyHeadLens =
  lens
    L.NE.head
    (\(_x :| xs) newX -> newX :| xs)

currentSource :: CharSourceStack -> CharSource.CharSource
currentSource = L.NE.head . unCharSourceStack

endCurrentCharSource :: CharSourceStack -> Maybe CharSourceStack
endCurrentCharSource stack = case L.NE.uncons stack.unCharSourceStack of
  (_source, mayRest) -> case mayRest of
    Nothing -> Nothing
    Just rest -> Just (stack {unCharSourceStack = rest})

stackIsFinished :: CharSourceStack -> Bool
stackIsFinished stack =
  all CharSource.sourceIsFinished stack.unCharSourceStack
