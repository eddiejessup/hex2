module Hex.Common.HexInput.Interface where

import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as L.NE
import Formatting qualified as F
import Hex.Common.Codes qualified as Code
import Hex.Common.Token.Lexed qualified as LT
import Hexlude

data LoadedCharSource = OpenLoadedCharSource OpenCharSource | FinishedLoadedCharSource
  deriving stock (Show, Generic)

-- The input to Tex is a sequence of “lines.”
data OpenCharSource = OpenCharSource
  { -- | The line number of the current line.
    lineNr :: LineNr,
    -- | The line currently being read, after processing.
    currentLine :: HexLine,
    lineState :: LineState,
    sourceLines :: [ByteString] -- Remaining lines, in unprocessed form.
  }
  deriving stock (Show, Generic)

charSourceLineNr :: LoadedCharSource -> Maybe LineNr
charSourceLineNr = \case
  OpenLoadedCharSource openCharSource -> Just openCharSource.lineNr
  FinishedLoadedCharSource -> Nothing

-- | A source line-number
newtype LineNr = LineNr {unLineNr :: Int}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Enum)

fmtMayLineNr :: Fmt (Maybe LineNr)
fmtMayLineNr = F.maybed "?" fmtLineNr

fmtLineNr :: Fmt LineNr
fmtLineNr = F.later $ \ln ->
  F.bformat (F.left 4 ' ') ln.unLineNr

data LineState
  = SkippingBlanks
  | LineMiddle
  | LineBegin
  deriving stock (Eq, Show)

data HexLineToken
  = CurrentLineByte Word8
  | CurrentLineLexToken LT.LexToken
  deriving stock (Show, Eq, Generic)

-- Represents a line of input, with trailing spaces removed and \endlinechar
-- appended.
data HexLine = HexLine {unHexLine :: [HexLineToken]}
  deriving stock (Show, Generic)

-- Tex deletes any ⟨space⟩ characters (number 32) that occur at the right end of
-- an input line. Then it inserts a ⟨return⟩ character (number 13) at the right
-- end of the line, except that it places nothing additional at the end of a
-- line that you inserted with ‘I’ during error recovery. Note that ⟨return⟩ is
-- considered to be an actual character that is part of the line; you can obtain
-- special effects by changing its catcode.
mkHexLine :: Maybe Code.CharCode -> ByteString -> HexLine
mkHexLine mayEndLineChar bs =
  let spaceStripped = case BS.findIndexEnd ((/= 32)) bs of
        Nothing -> ""
        Just i -> BS.take (i + 1) bs
      withEndLine = case mayEndLineChar of
        Nothing -> spaceStripped
        Just endLineChar -> spaceStripped <> BS.singleton (endLineChar.unCharCode)
   in HexLine $ CurrentLineByte <$> BS.unpack withEndLine

newCharSource :: Maybe Code.CharCode -> NonEmpty ByteString -> LoadedCharSource
newCharSource mayEndLineChar sourceLines =
  OpenLoadedCharSource
    OpenCharSource
      { lineNr = LineNr 1,
        currentLine = mkHexLine mayEndLineChar (L.NE.head sourceLines),
        sourceLines = L.NE.tail sourceLines,
        lineState = LineBegin
      }

endCurrentLineImpl :: Maybe Code.CharCode -> OpenCharSource -> LoadedCharSource
endCurrentLineImpl mayEndLineChar a =
  -- Get the next line from the unprocessed list.
  case uncons a.sourceLines of
    -- If no lines left, the char-source is exhausted.
    Nothing ->
      FinishedLoadedCharSource
    -- Otherwise, increment the line number, set the current line to the line we
    -- just got, after processing, and set the remainder as the remaining lines.
    Just (nextLine, restOfLines) ->
      OpenLoadedCharSource $
        a
          & #lineNr %~ succ
          & #currentLine .~ mkHexLine mayEndLineChar nextLine
          & #sourceLines .~ restOfLines
          & #lineState .~ LineBegin

class Monad m => MonadHexInput m where
  endCurrentLine :: m ()

  sourceIsFinished :: m Bool

  getSource :: m LoadedCharSource

  putSource :: LoadedCharSource -> m ()

  insertLexToken :: LT.LexToken -> m ()

  insertLexTokens :: Seq LT.LexToken -> m ()

-- getEndLineCharCode :: HSt.MonadHexState m => m (Maybe Code.CharCode)
-- getEndLineCharCode =
--   Code.fromHexInt <$> HSt.getParameterValue (HSt.Param.IntQuantParam HSt.Param.EndLineChar)

-- sourceIsFinishedImpl ::
--   ( MonadState st m,
--     HasType LoadedCharSource st
--   ) =>
--   m Bool
-- sourceIsFinishedImpl = do
--   use (typed @LoadedCharSource) <&> \case
--     Cat.LoadedCharSource.FinishedLoadedCharSource -> True
--     Cat.LoadedCharSource.OpenLoadedCharSource _ -> False

-- endCurrentLineImpl ::
--   ( MonadState st m,
--     HasType LoadedCharSource st,
--     HSt.MonadHexState m
--   ) =>
--   m ()
-- endCurrentLineImpl = do
--   use (typed @LoadedCharSource) >>= \case
--     Cat.LoadedCharSource.FinishedLoadedCharSource -> pure ()
--     Cat.LoadedCharSource.OpenLoadedCharSource openCharSource -> do
--       -- End the line, update the char-source to the new state
--       newSource <- Cat.LoadedCharSource.endCurrentLine <$> getEndLineCharCode <*> pure openCharSource
--       assign' (typed @LoadedCharSource) newSource

-- withOpenCharSource ::
--   ( MonadState st m,
--     HasType LoadedCharSource st
--   ) =>
--   (Cat.LoadedCharSource.OpenCharSource -> m (Maybe a)) ->
--   m (Maybe a)
-- withOpenCharSource k = do
--   use (typed @LoadedCharSource) >>= \case
--     Cat.LoadedCharSource.FinishedLoadedCharSource ->
--       pure Nothing
--     Cat.LoadedCharSource.OpenLoadedCharSource openCharSource ->
--       k openCharSource
