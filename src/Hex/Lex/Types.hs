{-# LANGUAGE PolyKinds #-}

module Hex.Lex.Types where

import Data.ByteString qualified as BS
import Data.Generics.Product.HList qualified as G.P.HList
import Data.Generics.Product.Internal.HList qualified as G.P.HList
import Hex.Categorise.Types as H.Cat
import Hex.Codes qualified as H.Codes
import Hexlude

newtype ControlSequence = ControlSequence ByteString
  deriving stock (Show, Generic)
  deriving newtype (Eq, Hashable)

mkControlSequence :: [H.Codes.CharCode] -> ControlSequence
mkControlSequence csChars = ControlSequence $ BS.pack $ H.Codes.unCharCode <$> csChars

data LexCharCat = LexCharCat
  { lexCCChar :: H.Codes.CharCode,
    lexCCCat :: H.Codes.CoreCatCode
  }
  deriving stock (Show, Eq, Generic)

data LexToken
  = CharCatLexToken LexCharCat
  | ControlSequenceLexToken ControlSequence
  deriving stock (Show, Eq, Generic)

lexTokCharCode :: AffineTraversal' LexToken H.Codes.CharCode
lexTokCharCode = lexTokCharCat % typed @H.Codes.CharCode

lexTokCategory :: AffineTraversal' LexToken H.Codes.CoreCatCode
lexTokCategory = lexTokCharCat % typed @H.Codes.CoreCatCode

lexTokCharCat :: AffineTraversal' LexToken LexCharCat
lexTokCharCat = castOptic @An_AffineTraversal (_Ctor @"CharCatLexToken")

lexTokCharCatTup :: AffineTraversal' LexToken (H.Codes.CharCode, H.Codes.CoreCatCode)
lexTokCharCatTup = lexTokCharCat % prodTupleIso

prodTupleIso :: (Generic s, G.P.HList.GIsList (Rep s) (Rep s) as as, G.P.HList.ListTuple t t as as) => Iso' s t
prodTupleIso = G.P.HList.list % listTupleIso

listTupleIso :: forall (t :: Type) (as :: [Type]). G.P.HList.ListTuple t t as as => Iso' (G.P.HList.HList as) t
listTupleIso = iso (G.P.HList.listToTuple @t @t @as @as) (G.P.HList.tupleToList @t @t @as @as)

listTupleIso2 :: forall a b. Iso' (G.P.HList.HList '[a, b]) (a, b)
listTupleIso2 = iso (G.P.HList.listToTuple @(a, b) @(a, b) @'[a, b] @'[a, b]) (G.P.HList.tupleToList @(a, b) @(a, b) @'[a, b] @'[a, b])

data LexState
  = SkippingBlanks
  | LineMiddle
  | LineBegin
  deriving stock (Eq, Show)

data LexError
  = TerminalEscapeCharacter
  | InvalidCharacter
  deriving stock (Show, Eq, Generic)

data LexFailure = LexEndOfInputFailure H.Cat.EndOfInput | LexErrorFailure LexError
  deriving stock (Show, Eq, Generic)

parToken :: LexToken
parToken = ControlSequenceLexToken $ mkControlSequence $ H.Codes.unsafeCodeFromChar <$> ("par" :: [Char])
