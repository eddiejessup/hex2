module Hex.Common.HexIO.Impl.IOState where

import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import Hex.Common.Codes qualified as Code
import Hex.Common.HexIO.Impl.CharSourceStack (CharSourceStack)
import Hex.Common.HexIO.Impl.CharSourceStack qualified as SourceStack
import Hex.Common.HexIO.Interface qualified as HIO
import Hex.Common.Quantity qualified as Q
import Hexlude

data IOState = IOState
  { sourceStack :: CharSourceStack,
    inputStreams :: StreamVec,
    outputStreams :: StreamVec
  }
  deriving stock (Show, Generic)

newtype StreamVec = StreamVec {unStreamVec :: Vector (Maybe Handle)}
  deriving stock (Show, Generic)

newStreamVec :: StreamVec
newStreamVec = StreamVec $ V.replicate 16 Nothing

streamVecIx :: Q.FourBitInt -> Lens' StreamVec (Maybe Handle)
streamVecIx streamNr = lens getter setter
  where
    streamNrInt = streamNr.unFourBitInt.unHexInt

    getter (StreamVec mayHandles) =
      mayHandles ! streamNrInt

    setter (StreamVec mayHandles) x =
      StreamVec $ V.unsafeUpd mayHandles [(streamNrInt, x)]

toIOMode :: HIO.InputOrOutput -> IOMode
toIOMode = \case
  HIO.InputFile -> ReadMode
  HIO.OutputFile -> WriteMode

streamsLens :: HIO.InputOrOutput -> Lens' IOState StreamVec
streamsLens = \case
  HIO.InputFile -> #inputStreams
  HIO.OutputFile -> #outputStreams

streamLens :: HIO.InputOrOutput -> Q.FourBitInt -> Lens' IOState (Maybe Handle)
streamLens inOrOut streamNr = streamsLens inOrOut % streamVecIx streamNr

newIOState ::
  Maybe Code.CharCode ->
  ByteString ->
  IOState
newIOState mayEndLineChar bs = do
  let sourceStack = SourceStack.newCharSourceStack mayEndLineChar bs
  IOState
    { sourceStack,
      inputStreams = newStreamVec,
      outputStreams = newStreamVec
    }
