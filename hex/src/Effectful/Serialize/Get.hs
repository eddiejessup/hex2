{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}

module Effectful.Serialize.Get where

import Data.ByteString qualified as B
import Data.ByteString.Unsafe qualified as B
import Effectful
import Formatting qualified as F
import GHC.Base hiding (($!))
import GHC.Num (Num ((*), (+), (-)))
import GHC.Word
import Hexlude

data Get :: Effect where
  GetWord8 :: Get m Word8
  GetInt8 :: Get m Int8
  -- BE
  GetWord16be :: Get m Word16
  GetWord32be :: Get m Word32
  GetWord64be :: Get m Word64
  GetInt16be :: Get m Int16
  GetInt32be :: Get m Int32
  GetInt64be :: Get m Int64
  -- LE
  GetWord16le :: Get m Word16
  GetWord32le :: Get m Word32
  GetWord64le :: Get m Word64
  GetInt16le :: Get m Int16
  GetInt32le :: Get m Int32
  GetInt64le :: Get m Int64
  -- Bytes.
  GetBytes :: Int -> Get m ByteString
  -- Introspection.
  GetBytesRead :: Get m Int
  -- Misc.
  Skip :: Int -> Get m ()

makeEffect ''Get

getWords :: Get :> es => Int -> Eff es ByteString
getWords = getBytes . wordToByte

wordToByte :: (Num a) => a -> a
wordToByte = (* 4)

runGet :: (Error Text :> es) => ByteString -> Eff (Get : es) a -> Eff es a
runGet input = reinterpret (evalStateLocal (newGetState input)) $ \_ -> \case
  GetWord8 -> getWord8Impl
  GetInt8 -> fromIntegral @Word8 @Int8 <$> getWord8Impl
  GetWord16be -> getWord16beImpl
  GetWord32be -> getWord32beImpl
  GetWord64be -> getWord64beImpl
  GetInt16be -> getInt16beImpl
  GetInt32be -> getInt32beImpl
  GetInt64be -> getInt64beImpl
  GetWord16le -> getWord16leImpl
  GetWord32le -> getWord32leImpl
  GetWord64le -> getWord64leImpl
  GetInt16le -> getInt16leImpl
  GetInt32le -> getInt32leImpl
  GetInt64le -> getInt64leImpl
  GetBytes n -> getBytesImpl n
  GetBytesRead -> getBytesReadImpl
  Skip n -> skipImpl n

data GetState = GetState
  { input :: ByteString,
    bytesRead :: Int
  }
  deriving stock (Eq, Show, Generic)

newGetState :: ByteString -> GetState
newGetState input = GetState {input, bytesRead = 0}

getWord8Impl :: (Error Text :> es, State GetState :> es) => Eff es Word8
getWord8Impl = do
  s <- getBytesImpl 1
  pure (B.unsafeHead s)

getWord16beImpl :: (Error Text :> es, State GetState :> es) => Eff es Word16
getWord16beImpl = do
  s <- getBytesImpl 2
  pure $!
    (fromIntegral (s `B.unsafeIndex` 0) `shiftl_w16` 8)
      .|. fromIntegral (s `B.unsafeIndex` 1)

getWord32beImpl :: (Error Text :> es, State GetState :> es) => Eff es Word32
getWord32beImpl = do
  s <- getBytesImpl 4
  pure $!
    (fromIntegral (s `B.unsafeIndex` 0) `shiftl_w32` 24)
      .|. (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w32` 16)
      .|. (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w32` 8)
      .|. fromIntegral (s `B.unsafeIndex` 3)

getWord16leImpl :: (Error Text :> es, State GetState :> es) => Eff es Word16
getWord16leImpl = do
  s <- getBytesImpl 2
  pure $!
    (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w16` 8)
      .|. fromIntegral (s `B.unsafeIndex` 0)

-- | Read a Word32 in little endian format
getWord32leImpl :: (Error Text :> es, State GetState :> es) => Eff es Word32
getWord32leImpl = do
  s <- getBytesImpl 4
  pure $!
    (fromIntegral (s `B.unsafeIndex` 3) `shiftl_w32` 24)
      .|. (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w32` 16)
      .|. (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w32` 8)
      .|. fromIntegral (s `B.unsafeIndex` 0)

-- | Read a Word64 in big endian format
getWord64beImpl :: (Error Text :> es, State GetState :> es) => Eff es Word64
getWord64beImpl = do
  s <- getBytesImpl 8
  pure $!
    (fromIntegral (s `B.unsafeIndex` 0) `shiftl_w64` 56)
      .|. (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w64` 48)
      .|. (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w64` 40)
      .|. (fromIntegral (s `B.unsafeIndex` 3) `shiftl_w64` 32)
      .|. (fromIntegral (s `B.unsafeIndex` 4) `shiftl_w64` 24)
      .|. (fromIntegral (s `B.unsafeIndex` 5) `shiftl_w64` 16)
      .|. (fromIntegral (s `B.unsafeIndex` 6) `shiftl_w64` 8)
      .|. fromIntegral (s `B.unsafeIndex` 7)

-- | Read a Int16 in big endian format
getInt16beImpl :: (Error Text :> es, State GetState :> es) => Eff es Int16
getInt16beImpl = do
  s <- getBytesImpl 2
  pure $!
    fromIntegral (s `B.unsafeIndex` 0) `shiftL` 8
      .|. fromIntegral (s `B.unsafeIndex` 1)

-- | Read a Int16 in little endian format
getInt16leImpl :: (Error Text :> es, State GetState :> es) => Eff es Int16
getInt16leImpl = do
  s <- getBytesImpl 2
  pure $!
    fromIntegral (s `B.unsafeIndex` 1) `shiftL` 8
      .|. fromIntegral (s `B.unsafeIndex` 0)

-- | Read a Int32 in big endian format
getInt32beImpl :: (Error Text :> es, State GetState :> es) => Eff es Int32
getInt32beImpl = do
  s <- getBytesImpl 4
  pure $!
    fromIntegral (s `B.unsafeIndex` 0) `shiftL` 24
      .|. fromIntegral (s `B.unsafeIndex` 1) `shiftL` 16
      .|. fromIntegral (s `B.unsafeIndex` 2) `shiftL` 8
      .|. fromIntegral (s `B.unsafeIndex` 3)

-- | Read a Int32 in little endian format
getInt32leImpl :: (Error Text :> es, State GetState :> es) => Eff es Int32
getInt32leImpl = do
  s <- getBytesImpl 4
  pure $!
    fromIntegral (s `B.unsafeIndex` 3) `shiftL` 24
      .|. fromIntegral (s `B.unsafeIndex` 2) `shiftL` 16
      .|. fromIntegral (s `B.unsafeIndex` 1) `shiftL` 8
      .|. fromIntegral (s `B.unsafeIndex` 0)

-- | Read a Int64 in big endian format
getInt64beImpl :: (Error Text :> es, State GetState :> es) => Eff es Int64
getInt64beImpl = do
  s <- getBytesImpl 8
  pure $!
    fromIntegral (s `B.unsafeIndex` 0) `shiftL` 56
      .|. fromIntegral (s `B.unsafeIndex` 1) `shiftL` 48
      .|. fromIntegral (s `B.unsafeIndex` 2) `shiftL` 40
      .|. fromIntegral (s `B.unsafeIndex` 3) `shiftL` 32
      .|. fromIntegral (s `B.unsafeIndex` 4) `shiftL` 24
      .|. fromIntegral (s `B.unsafeIndex` 5) `shiftL` 16
      .|. fromIntegral (s `B.unsafeIndex` 6) `shiftL` 8
      .|. fromIntegral (s `B.unsafeIndex` 7)

-- | Read a Int64 in little endian format
getInt64leImpl :: (Error Text :> es, State GetState :> es) => Eff es Int64
getInt64leImpl = do
  s <- getBytesImpl 8
  pure $!
    fromIntegral (s `B.unsafeIndex` 7) `shiftL` 56
      .|. fromIntegral (s `B.unsafeIndex` 6) `shiftL` 48
      .|. fromIntegral (s `B.unsafeIndex` 5) `shiftL` 40
      .|. fromIntegral (s `B.unsafeIndex` 4) `shiftL` 32
      .|. fromIntegral (s `B.unsafeIndex` 3) `shiftL` 24
      .|. fromIntegral (s `B.unsafeIndex` 2) `shiftL` 16
      .|. fromIntegral (s `B.unsafeIndex` 1) `shiftL` 8
      .|. fromIntegral (s `B.unsafeIndex` 0)

-- | Read a Word64 in little endian format
getWord64leImpl :: (Error Text :> es, State GetState :> es) => Eff es Word64
getWord64leImpl = do
  s <- getBytesImpl 8
  pure $!
    (fromIntegral (s `B.unsafeIndex` 7) `shiftl_w64` 56)
      .|. (fromIntegral (s `B.unsafeIndex` 6) `shiftl_w64` 48)
      .|. (fromIntegral (s `B.unsafeIndex` 5) `shiftl_w64` 40)
      .|. (fromIntegral (s `B.unsafeIndex` 4) `shiftl_w64` 32)
      .|. (fromIntegral (s `B.unsafeIndex` 3) `shiftl_w64` 24)
      .|. (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w64` 16)
      .|. (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w64` 8)
      .|. fromIntegral (s `B.unsafeIndex` 0)

-- | Pull @n@ bytes from the input, as a strict ByteString.
getBytesImpl :: (Error Text :> es, State GetState :> es) => Int -> Eff es ByteString
getBytesImpl n
  | n < 0 =
      throwError @Text "getBytes: negative length requested"
getBytesImpl n = do
  s <- ensureImpl n
  let consumed = B.unsafeTake n s
      rest = B.unsafeDrop n s
  assign @GetState #input rest
  modifying @GetState #bytesRead (+ n)
  pure consumed

{-# INLINE getBytes #-}

{-# INLINE ensureImpl #-}
ensureImpl :: (Error Text :> es, State GetState :> es) => Int -> Eff es ByteString
ensureImpl nNeeded = seq nNeeded $ do
  curInput <- use @GetState #input
  let nHave = B.length curInput
  when (nHave - nNeeded < 0) $
    throwError @Text $
      F.sformat ("Too few bytes: need " |%| F.int |%| ", have " |%| F.int) nNeeded nHave
  pure curInput

type Buffer = Maybe ByteString

extendBuffer :: Buffer -> ByteString -> Buffer
extendBuffer buf chunk = do
  bs <- buf
  pure $! bs `B.append` chunk
{-# INLINE extendBuffer #-}

getBytesReadImpl :: (State GetState :> es) => Eff es Int
getBytesReadImpl = use @GetState #bytesRead

-- | Skip ahead @n@ bytes. Fails if fewer than @n@ bytes are available.
skipImpl :: (State GetState :> es, Error Text :> es) => Int -> Eff es ()
skipImpl n = do
  s <- ensureImpl n
  assign @GetState #input (B.drop n s)
  modifying @GetState #bytesRead (+ n)

shiftl_w16 :: Word16 -> Int -> Word16
shiftl_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftLWord16#` i)

shiftl_w32 :: Word32 -> Int -> Word32
shiftl_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftLWord32#` i)

shiftl_w64 :: Word64 -> Int -> Word64
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL64#` i)
