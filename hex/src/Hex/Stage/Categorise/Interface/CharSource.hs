module Hex.Stage.Categorise.Interface.CharSource where

import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as L.NE
import Formatting qualified as F
import Hex.Common.Codes qualified as Code
import Hexlude

data LoadedCharSource = OpenLoadedCharSource OpenCharSource | FinishedLoadedCharSource
  deriving stock (Show, Generic)

-- The input to Tex is a sequence of “lines.”
data OpenCharSource = OpenCharSource
  { -- | The line number of the current line.
    lineNr :: LineNr,
    -- | The line currently being read, after processing.
    currentLine :: HexLine,
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

-- Represents a line of input, with trailing spaces removed and \endlinechar
-- appended.
data HexLine = HexLine {unHexLine :: ByteString}
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
   in HexLine $ case mayEndLineChar of
        Nothing -> spaceStripped
        Just endLineChar -> spaceStripped <> BS.singleton (endLineChar.unCharCode)

newCharSource :: Maybe Code.CharCode -> NonEmpty ByteString -> LoadedCharSource
newCharSource mayEndLineChar sourceLines =
  OpenLoadedCharSource
    OpenCharSource
      { lineNr = LineNr 1,
        currentLine = mkHexLine mayEndLineChar (L.NE.head sourceLines),
        sourceLines = L.NE.tail sourceLines
      }

endCurrentLine :: Maybe Code.CharCode -> OpenCharSource -> LoadedCharSource
endCurrentLine mayEndLineChar a =
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
