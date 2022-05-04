module Hex.Stage.Lex.Interface.Extract where

import Data.ByteString qualified as BS
import Data.Generics.Product (getTyped)
import Data.Generics.Product.HList qualified as G.P.HList
import Data.Generics.Product.Internal.HList qualified as G.P.HList
import Data.Text.Encoding qualified as Tx
import Formatting qualified as F
import Hex.Common.Codes qualified as Code
import Hex.Common.Codes qualified as Codes
import Hexlude

newtype ControlSequence = ControlSequence ByteString
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)

mkControlSequence :: [Codes.CharCode] -> ControlSequence
mkControlSequence csChars = ControlSequence $ BS.pack $ Codes.unCharCode <$> csChars

fmtControlSequence :: Fmt ControlSequence r
fmtControlSequence = "\\" F.% (F.accessed (Tx.decodeUtf8 . (getTyped @ByteString)) F.stext)

data LexCharCat = LexCharCat
  { lexCCChar :: Codes.CharCode,
    lexCCCat :: Codes.CoreCatCode
  }
  deriving stock (Show, Eq, Generic)

fmtLexCharCat :: Fmt LexCharCat r
fmtLexCharCat =
  let f1 = F.accessed (.lexCCChar) Code.fmtCharCode
      f2 = F.accessed (.lexCCCat) Code.fmtCoreCatCode
   in F.parenthesised $ f1 <> F.fconst ", " <> f2

data LexToken
  = CharCatLexToken LexCharCat
  | ControlSequenceLexToken ControlSequence
  deriving stock (Show, Eq, Generic)

fmtLexToken :: Fmt LexToken r
fmtLexToken = later $ \case
  CharCatLexToken lexCC ->
    bformat fmtLexCharCat lexCC
  ControlSequenceLexToken controlSeq ->
    bformat fmtControlSequence controlSeq

lexTokCharCode :: AffineTraversal' LexToken Codes.CharCode
lexTokCharCode = lexTokCharCat % typed @Codes.CharCode

lexTokCategory :: AffineTraversal' LexToken Codes.CoreCatCode
lexTokCategory = lexTokCharCat % typed @Codes.CoreCatCode

lexTokCharCat :: AffineTraversal' LexToken LexCharCat
lexTokCharCat = castOptic @An_AffineTraversal (_Ctor @"CharCatLexToken")

lexTokCharCatTup :: AffineTraversal' LexToken (Codes.CharCode, Codes.CoreCatCode)
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

parToken :: LexToken
parToken = ControlSequenceLexToken $ mkControlSequence $ Codes.unsafeCodeFromChar <$> ("par" :: [Char])
