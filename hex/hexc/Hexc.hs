module Main where

import Data.ByteString qualified as BS
import Hex.Run.App qualified as Run
import Hex.Run.Categorise qualified as Run.Cat
import Hex.Run.Lex qualified as Run.Lex
import Hex.Run.Resolve qualified as Run.Resolve
import Hex.Run.Expand qualified as Run.Expand
import Hex.Run.Parse qualified as Run.Parse
import Hexlude
import Options.Applicative
import Hex.Stage.Resolve.Interface (ResolutionMode(..))
import qualified Hex.Run.Evaluate as Run.Evaluate
import Hex.Stage.Interpret.Build.List.Elem (fmtVList)
import qualified Hex.Run.Interpret as Run.Interpret

data Input = FileInput FilePath | StdInput

fileInputParser :: Parser Input
fileInputParser =
  FileInput
    <$> strOption
      ( long "file"
          <> short 'f'
          <> metavar "FILENAME"
          <> help "Input file"
      )

dviRunParser :: Parser DVIWriteOptions
dviRunParser =
  DVIWriteOptions
    <$> strOption (long "file" <> short 'f' <> metavar "FILENAME" <> help "Output file")

stdInputParser :: Parser Input
stdInputParser =
  flag'
    StdInput
    ( long "stdin"
        <> help "Read from stdin"
    )

inputParser :: Parser Input
inputParser = fileInputParser <|> stdInputParser

data RunMode
  = CatMode
  | LexMode
  | ResolveMode ResolutionMode
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
        <> command "resolve" (info (pure (ResolveMode Resolving)) (progDesc ""))
        <> command "resolve_not" (info (pure (ResolveMode NotResolving)) (progDesc ""))
        <> command "expand" (info (pure ExpandMode) (progDesc ""))
        <> command "raw-command" (info (pure RawCommandMode) (progDesc ""))
        <> command "eval-command" (info (pure EvalCommandMode) (progDesc ""))
        <> command "vlist" (info (pure VListMode) (progDesc ""))
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

main :: IO ()
main = do
  opts <- execParser appOptionsParserInfo
  (input, _maybeInPath) <-
    case input opts of
      StdInput -> do
        cs <- liftIO BS.getContents
        pure (cs, Nothing)
      FileInput inPathStr -> do
        cs <- BS.readFile inPathStr
        pure (cs, Just inPathStr)
  case opts.mode of
    CatMode -> do
      ccs <- Run.unsafeEvalApp input Run.Cat.categoriseAll
      putText $ sformat Run.Cat.fmtCategoriseResult ccs
    LexMode -> do
      lts <- Run.unsafeEvalApp input Run.Lex.lexAll
      putText $ sformat Run.Lex.fmtLexResult lts
    ResolveMode resMode -> do
      resultList <- Run.unsafeEvalApp input (Run.Resolve.resolveAll resMode)
      putText $ sformat Run.Resolve.fmtResolveResult resultList
    ExpandMode -> do
      resultList <- Run.unsafeEvalApp input Run.Expand.expandAll
      putText $ sformat Run.Expand.fmtExpandResult resultList
    RawCommandMode -> do
      commandList <- Run.unsafeEvalApp input Run.Parse.parseAll
      putText $ sformat Run.Parse.fmtCommandList commandList
    EvalCommandMode -> do
      commandList <- Run.unsafeEvalApp input Run.Evaluate.evaluateAll
      putText $ sformat Run.Evaluate.fmtEvalCommandList commandList
    VListMode -> do
      vList <- Run.unsafeEvalApp input Run.Interpret.interpretAll
      putText $ sformat fmtVList vList
    _ ->
      putText $ "Unsupported mode: " <> show (opts.mode)
  pure ()
