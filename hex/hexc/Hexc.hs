module Main where

import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as L.NE
import Hex.Run.App qualified as Run
import Hex.Run.Categorise qualified as Run.Cat
import Hex.Run.Evaluate qualified as Run.Evaluate
import Hex.Run.Expand qualified as Run.Expand
import Hex.Run.Interpret qualified as Run.Interpret
import Hex.Run.Lex qualified as Run.Lex
import Hex.Run.Parse qualified as Run.Parse
import Hex.Run.Resolve qualified as Run.Resolve
import Hex.Stage.Interpret.Build.List.Elem (fmtHListMultiLine, fmtVList)
import Hexlude
import Hexlude qualified as Tx
import Options.Applicative

data Input = FileInput FilePath | StdInput | ExpressionInput Text

inputParser :: Parser Input
inputParser = fileInputParser <|> stdInputParser <|> expressionParser
  where
    fileInputParser :: Parser Input
    fileInputParser =
      FileInput
        <$> strOption
          ( long "file"
              <> short 'f'
              <> metavar "FILENAME"
              <> help "Input file"
          )

    stdInputParser :: Parser Input
    stdInputParser =
      flag'
        StdInput
        ( long "stdin"
            <> help "Read input from stdin"
        )

    expressionParser :: Parser Input
    expressionParser =
      ExpressionInput
        <$> strOption
          ( long "expr"
              <> short 'e'
              <> metavar "EXPRESSION"
              <> help "Input expression"
          )

dviRunParser :: Parser DVIWriteOptions
dviRunParser =
  DVIWriteOptions
    <$> strOption (long "file" <> short 'f' <> metavar "FILENAME" <> help "Output file")

data RunMode
  = CatMode
  | LexMode
  | ResolveMode
  | ExpandMode
  | RawCommandMode
  | EvalCommandMode
  | ParaListMode
  | ParaSetMode
  | VListMode
  | PageMode
  | SemanticDVIInstructionsMode
  | RawDVIInstructionsMode
  | DVIWriteMode DVIWriteOptions
  deriving stock (Show)

data AppOptions = AppOptions
  { mode :: RunMode,
    input :: Input,
    searchDirs :: [FilePath],
    withAmbles :: Bool
  }

data DVIWriteOptions = DVIWriteOptions
  { dviOutputPath :: FilePath
  }
  deriving stock (Show)

runModeParser :: Parser RunMode
runModeParser =
  subparser
    ( command "cat" (info (pure CatMode) (progDesc ""))
        <> command "lex" (info (pure LexMode) (progDesc ""))
        <> command "resolve" (info (pure ResolveMode) (progDesc ""))
        <> command "expand" (info (pure ExpandMode) (progDesc ""))
        <> command "raw-command" (info (pure RawCommandMode) (progDesc ""))
        <> command "eval-command" (info (pure EvalCommandMode) (progDesc ""))
        <> command "vlist" (info (pure VListMode) (progDesc ""))
        <> command "paralist" (info (pure ParaListMode) (progDesc ""))
    )

appOptionsParser :: Parser AppOptions
appOptionsParser =
  AppOptions
    <$> runModeParser
    <*> inputParser
    <*> many (strOption (long "dir" <> short 'd' <> metavar "SEARCH_DIR" <> help "Directory to search for support files"))
    <*> switch (long "amble" <> short 'a' <> help "Surround input with pre- and post-amble")

appOptionsParserInfo :: ParserInfo AppOptions
appOptionsParserInfo =
  info
    (appOptionsParser <**> helper)
    ( fullDesc
        <> progDesc "Run Hex source"
        <> header "Hex"
    )

newlineWord :: Word8
newlineWord = 10

main :: IO ()
main = do
  opts <- execParser appOptionsParserInfo
  (inputBytes, _maybeInPath) <-
    case opts.input of
      StdInput -> do
        cs <- liftIO BS.getContents
        pure (cs, Nothing)
      ExpressionInput csText -> do
        let cs = Tx.encodeUtf8 csText
        pure (cs, Nothing)
      FileInput inPathStr -> do
        cs <- BS.readFile inPathStr
        pure (cs, Just inPathStr)
  -- Split the input into lines, assuming '\n' line-termination characters.
  -- We will append the \endlinechar to each input line as we traverse the lines.
  -- We need at least one input line, to be the 'current line'.
  inputLines <- note (panic "No lines of input")
    (L.NE.nonEmpty (BS.split newlineWord inputBytes))
  case opts.mode of
    CatMode -> do
      ccs <- Run.unsafeEvalApp inputLines Run.Cat.categoriseAll
      putText $ sformat Run.Cat.fmtCategoriseResult ccs
    LexMode -> do
      lts <- Run.unsafeEvalApp inputLines Run.Lex.lexAll
      putText $ sformat Run.Lex.fmtLexResult lts
    ResolveMode -> do
      resultList <- Run.unsafeEvalApp inputLines Run.Resolve.resolveAll
      putText $ sformat Run.Resolve.fmtResolveResult resultList
    ExpandMode -> do
      resultList <- Run.unsafeEvalApp inputLines Run.Expand.expandAll
      putText $ sformat Run.Expand.fmtExpandResult resultList
    RawCommandMode -> do
      commandList <- Run.unsafeEvalApp inputLines Run.Parse.parseAll
      putText $ sformat Run.Parse.fmtCommandList commandList
    EvalCommandMode -> do
      commandList <- Run.unsafeEvalApp inputLines Run.Evaluate.evaluateAll
      putText $ sformat Run.Evaluate.fmtEvalCommandList commandList
    VListMode -> do
      vList <- Run.unsafeEvalApp inputLines Run.Interpret.interpretInMainVMode
      putText $ sformat fmtVList vList
    ParaListMode -> do
      (endReason, paraList) <- Run.unsafeEvalApp inputLines Run.Interpret.interpretInParaMode
      putText $ "End reason: " <> show endReason
      putText $ sformat fmtHListMultiLine paraList
    _ ->
      putText $ "Unsupported mode: " <> show (opts.mode)
  pure ()
