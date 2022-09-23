module Hex.Common.HexIO.Impl.IOState where

import Data.Vector (Vector)
import Hex.Common.Codes qualified as Code
import Hex.Common.HexIO.Impl.CharSourceStack (CharSourceStack)
import Hex.Common.HexIO.Impl.CharSourceStack qualified as SourceStack
import Hex.Common.HexIO.Interface qualified as HIO
import Hexlude

data IOState = IOState
  { sourceStack :: CharSourceStack,
    inputStreams :: Vector (Maybe Handle),
    outputStreams :: Vector (Maybe Handle)
  }
  deriving stock (Show, Generic)

toIOMode :: HIO.InputOrOutput -> IOMode
toIOMode = \case
  HIO.InputFile -> ReadMode
  HIO.OutputFile -> WriteMode

streamsLens :: HIO.InputOrOutput -> Lens' IOState (Vector (Maybe Handle))
streamsLens = \case
  HIO.InputFile -> #inputStreams
  HIO.OutputFile -> #outputStreams

newIOState ::
  Maybe Code.CharCode ->
  ByteString ->
  IOState
newIOState mayEndLineChar bs = do
  let sourceStack = SourceStack.newCharSourceStack mayEndLineChar bs
  IOState
    { sourceStack,
      inputStreams = mempty,
      outputStreams = mempty
    }
