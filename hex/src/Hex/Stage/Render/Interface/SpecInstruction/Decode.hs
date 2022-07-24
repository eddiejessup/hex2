module Hex.Stage.Render.Interface.SpecInstruction.Decode where

import Data.ByteString.Builder qualified as BS.Builder
import Data.Serialize qualified as B
import Hexlude

data SignedNByteInt
  = Signed1ByteInt Int8
  | Signed2ByteInt Int16
  | Signed4ByteInt Int32
  deriving stock (Show, Eq, Generic)

data UnsignedNByteInt
  = Unsigned1ByteInt Word8
  | Unsigned2ByteInt Word16
  | Unsigned4ByteInt Word32
  deriving stock (Show, Eq, Generic)

data ByteLength = OneByte | TwoByte | FourByte
  deriving stock (Show)

sLength :: SignedNByteInt -> ByteLength
sLength = \case
  Signed1ByteInt _ -> OneByte
  Signed2ByteInt _ -> TwoByte
  Signed4ByteInt _ -> FourByte

uLength :: UnsignedNByteInt -> ByteLength
uLength = \case
  Unsigned1ByteInt _ -> OneByte
  Unsigned2ByteInt _ -> TwoByte
  Unsigned4ByteInt _ -> FourByte

instance B.Serialize SignedNByteInt where
  put = B.putBuilder . signedNByteIntBuilder
    where
      signedNByteIntBuilder :: SignedNByteInt -> BS.Builder.Builder
      signedNByteIntBuilder = \case
        Signed1ByteInt i8 -> BS.Builder.int8 i8
        Signed2ByteInt i16 -> BS.Builder.int16BE i16
        Signed4ByteInt i32 -> BS.Builder.int32BE i32

instance B.Serialize UnsignedNByteInt where
  put = B.putBuilder . unsignedNByteIntBuilder
    where
      unsignedNByteIntBuilder :: UnsignedNByteInt -> BS.Builder.Builder
      unsignedNByteIntBuilder = \case
        Unsigned1ByteInt w8 -> BS.Builder.word8 w8
        Unsigned2ByteInt w16 -> BS.Builder.word16BE w16
        Unsigned4ByteInt w32 -> BS.Builder.word32BE w32

-- >>> intToUnsigned (-1) == Nothing
-- True
-- >>> intToUnsigned (1) == Just (U1 (1))
-- True
-- >>> intToUnsigned (2^32) == Nothing
-- True
-- >>> intToUnsigned (2^32 - 1) == (Just (U4 (2^32 - 1)))
-- True
intToUnsigned ::
  Int ->
  Maybe UnsignedNByteInt
intToUnsigned n =
  case intToWord8 n of
    Just w -> Just $ Unsigned1ByteInt w
    Nothing ->
      case intToWord16 n of
        Just w -> Just $ Unsigned2ByteInt w
        Nothing ->
          case intToWord32 n of
            Just w -> Just $ Unsigned4ByteInt w
            Nothing ->
              Nothing

-- >>> intToSigned (-1) == Just (Signed1ByteInt (-1))
-- True
-- >>> intToSigned (1) == Just (Signed1ByteInt (1))
-- True
-- >>> intToSigned (2^32 - 1) == Nothing
-- True
intToSigned ::
  Int ->
  Maybe SignedNByteInt
intToSigned n =
  case intToInt8 n of
    Just w -> Just $ Signed1ByteInt w
    Nothing ->
      case intToInt16 n of
        Just w -> Just $ Signed2ByteInt w
        Nothing ->
          case intToInt32 n of
            Just w -> Just $ Signed4ByteInt w
            Nothing ->
              Nothing

-- >>> isJust $ intToWord8 2
-- True
-- >>> isJust $ intToWord8 0
-- True
-- >>> isNothing $ intToWord8 (-1)
-- True
-- >>> isJust $ intToWord8 255
-- True
-- >>> isNothing $ intToWord8 256
-- True
intToWord8 :: Int -> Maybe Word8
intToWord8 = toIntegralSized

-- >>> isNothing $ intToWord16 (-1)
-- True
-- >>> isJust $ intToWord16 0
-- True
-- >>> isJust $ intToWord16 65_535
-- True
-- >>> isNothing $ intToWord16 65_536
-- True
intToWord16 :: Int -> Maybe Word16
intToWord16 = toIntegralSized

-- >>> isNothing $ intToWord32 (-1)
-- True
-- >>> isJust $ intToWord32 0
-- True
-- >>> isJust $ intToWord32 4_294_967_295
-- True
-- >>> isNothing $ intToWord32 4_294_967_296
-- True
intToWord32 :: Int -> Maybe Word32
intToWord32 = toIntegralSized

-- >>> isJust $ intToInt8 (-1)
-- True
-- >>> isJust $ intToInt8 0
-- True
-- >>> isNothing $ intToInt8 128
-- True
-- >>> isJust $ intToInt8 (-128)
-- True
-- >>> isNothing $ intToInt8 (-129)
-- True
intToInt8 :: Int -> Maybe Int8
intToInt8 = toIntegralSized

-- >>> isJust $ intToInt16 (-1)
-- True
-- >>> isJust $ intToInt16 0
-- True
-- >>> isJust $ intToInt16 32_767
-- True
-- >>> isNothing $ intToInt16 32_768
-- True
-- >>> isJust $ intToInt16 (-32_768)
-- True
-- >>> isNothing $ intToInt16 (-32_769)
-- True
intToInt16 :: Int -> Maybe Int16
intToInt16 = toIntegralSized

-- >>> isJust $ intToInt32 (-1)
-- True
-- >>> isJust $ intToInt32 0
-- True
-- >>> isJust $ intToInt32 2_147_483_647
-- True
-- >>> isNothing $ intToInt32 2_147_483_648
-- True
-- >>> isJust $ intToInt32 (-2_147_483_648)
-- True
-- >>> isNothing $ intToInt32 (-2_147_483_649)
-- True
intToInt32 :: Int -> Maybe Int32
intToInt32 = toIntegralSized
