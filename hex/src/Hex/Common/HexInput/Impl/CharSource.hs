{-# LANGUAGE UndecidableInstances #-}

module Hex.Common.HexInput.Impl.CharSource where

import Data.ByteString qualified as BS
import Data.Sequence qualified as Seq
import Formatting qualified as F
import Hex.Common.Codes qualified as Code
import Hex.Common.Token.Lexed qualified as LT
import Hexlude

-- Non-Empty working line, non-empty source-lines: Midway through input
-- Empty working-line, non-empty source-lines: At end-of-working-line, ready to read next source-line.
-- Non-Empty working-line, empty source-lines: Midway through reading last line
-- Empty working-line, Empty source-lines: At end of input

-- The input to Tex is a sequence of “lines.”
data CharSource = CharSource
  { -- | The line number of the current line.
    lineNr :: LineNr,
    workingLine :: WorkingLine,
    sourceLines :: [ByteString] -- Remaining lines, in unprocessed form.
  }
  deriving stock (Show, Generic)

newlineWord :: Word8
newlineWord = 10

-- Split the input into lines, assuming '\n' line-termination characters.
-- We will append the \endlinechar to each input line as we traverse the lines.
newCharSource :: Maybe Code.CharCode -> ByteString -> CharSource
newCharSource mayEndLineChar sourceBytes =
  let (workingLineBytes, restLines) = case BS.split newlineWord sourceBytes of
        [] -> ("", [])
        fstLine : rest -> (fstLine, rest)
   in CharSource
        { lineNr = LineNr 1,
          workingLine = newWorkingLine mayEndLineChar workingLineBytes,
          sourceLines = restLines
        }

data WorkingLine = WorkingLine
  { sourceLine :: CurrentSourceLine,
    workingLexTokens :: Seq LT.LexToken
  }
  deriving stock (Show, Generic)

instance {-# OVERLAPPING #-} HasType WorkingLine st => HasType HexLine st where
  typed = typed @WorkingLine % #sourceLine % #currentLine

newWorkingLine :: Maybe Code.CharCode -> ByteString -> WorkingLine
newWorkingLine mayEndLineChar sourceLineBytes =
  WorkingLine
    { workingLexTokens = Seq.empty,
      sourceLine = newCurrentSourceLine mayEndLineChar sourceLineBytes
    }

workingLineIsFinished :: WorkingLine -> Bool
workingLineIsFinished workingLine =
  currentSourceLineIsFinished (workingLine.sourceLine)
    && Seq.null (workingLine.workingLexTokens)

data CurrentSourceLine = CurrentSourceLine
  { -- | The line currently being read, after processing.
    currentLine :: HexLine,
    lineState :: LineState
  }
  deriving stock (Show, Generic)

currentSourceLineIsFinished :: CurrentSourceLine -> Bool
currentSourceLineIsFinished = BS.null . unHexLine . (.currentLine)

newCurrentSourceLine :: Maybe Code.CharCode -> ByteString -> CurrentSourceLine
newCurrentSourceLine mayEndLineChar sourceLineBytes =
  CurrentSourceLine
    { currentLine = mkHexLine mayEndLineChar sourceLineBytes,
      lineState = LineBegin
    }

charSourceLineNr :: CharSource -> LineNr
charSourceLineNr = (.lineNr)

-- | A source line-number
newtype LineNr = LineNr {unLineNr :: Int}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Enum)

fmtLineNr :: Fmt LineNr
fmtLineNr = F.later $ \ln ->
  F.bformat (F.left 4 ' ') ln.unLineNr

data LineState
  = SkippingBlanks
  | LineMiddle
  | LineBegin
  deriving stock (Eq, Generic, Show)

fmtLineState :: Fmt LineState
fmtLineState = F.later $ \case
  SkippingBlanks -> "SkippingBlanks"
  LineMiddle -> "LineMiddle"
  LineBegin -> "LineBegin"

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
      withEndLine = case mayEndLineChar of
        Nothing -> spaceStripped
        Just endLineChar -> spaceStripped <> BS.singleton (endLineChar.unCharCode)
   in HexLine withEndLine

insertLexTokenToSource :: LT.LexToken -> CharSource -> CharSource
insertLexTokenToSource lt charSource = charSource & #workingLine % #workingLexTokens %~ cons lt

data HexLineToken
  = HexLineByte Word8
  | HexLineLexToken LT.LexToken
  deriving stock (Show, Eq, Generic)

endSourceCurrentLine :: Maybe Code.CharCode -> CharSource -> Maybe CharSource
endSourceCurrentLine mayEndLineChar a =
  -- Get the next line from the unprocessed list.
  case uncons a.sourceLines of
    -- If no lines left, the char-source is exhausted.
    Nothing ->
      Nothing
    -- Otherwise, increment the line number, set the current line to the line we
    -- just got, after processing, and set the remainder as the remaining lines.
    Just (nextLine, restOfLines) ->
      Just $
        a
          & #lineNr %~ succ
          & #workingLine .~ newWorkingLine mayEndLineChar nextLine
          & #sourceLines .~ restOfLines

sourceIsFinished :: CharSource -> Bool
sourceIsFinished charSource =
  workingLineIsFinished (charSource.workingLine)
    && null (charSource.sourceLines)
