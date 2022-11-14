module Main where

import Data.ByteString qualified as BS
import Data.Time qualified as Time
import Hex.Capability.Log.Interface qualified as Log
import Hex.Run.App qualified as Run
import Hex.Run.Evaluate qualified as Run.Evaluate
import Hex.Run.Expand qualified as Run.Expand
import Hex.Run.Interpret qualified as Run.Interpret
import Hex.Run.Lex qualified as Run.Lex
import Hex.Run.Paginate qualified as Run.Paginate
import Hex.Run.Parse qualified as Run.Parse
import Hex.Run.Render qualified as Run.Render
import Hex.Stage.Build.ListElem (fmtHListMultiLine, fmtVListElemSeq)
import Hex.Stage.Render.Interface.DocInstruction qualified as Render.Doc
import Hex.Stage.Render.Interface.SpecInstruction qualified as Render.Spec
import Hexlude
import Options.Applicative
import System.FilePath qualified as Path

data Input = FileInput FilePath | StdInput | ExpressionInput ByteString

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
    <$> strOption (long "out" <> short 'o' <> metavar "FILENAME" <> help "Output DVI file")

data RunMode
  = LexMode
  | ExpandMode
  | RawCommandMode
  | EvalCommandMode
  | ParaListMode
  | ParaSetMode
  | VListMode
  | PageMode
  | DVIDocInstructionMode
  | DVISpecInstructionMode
  | DVIWriteMode DVIWriteOptions
  deriving stock (Show)

data AppOptions = AppOptions
  { mode :: RunMode,
    input :: Input,
    searchDirs :: [FilePath],
    logLevel :: Log.LogLevel
  }

data DVIWriteOptions = DVIWriteOptions
  { dviOutputPath :: FilePath
  }
  deriving stock (Show)

runModeParser :: Parser RunMode
runModeParser =
  subparser
    ( command "lex" (info (pure LexMode) (progDesc ""))
        <> command "expand" (info (pure ExpandMode) (progDesc ""))
        <> command "raw-command" (info (pure RawCommandMode) (progDesc ""))
        <> command "eval-command" (info (pure EvalCommandMode) (progDesc ""))
        <> command "vlist" (info (pure VListMode) (progDesc ""))
        <> command "paralist" (info (pure ParaListMode) (progDesc ""))
        <> command "page" (info (pure PageMode) (progDesc ""))
        <> command "doc-dvi" (info (pure DVIDocInstructionMode) (progDesc ""))
        <> command "spec-dvi" (info (pure DVISpecInstructionMode) (progDesc ""))
        <> command "dvi" (info (DVIWriteMode <$> dviRunParser) (progDesc ""))
    )

appOptionsParser :: Parser AppOptions
appOptionsParser =
  AppOptions
    <$> runModeParser
    <*> inputParser
    <*> many (strOption (long "dir" <> short 'd' <> metavar "SEARCH_DIR" <> help "Directory to search for support files"))
    <*> option
      (maybeReader Log.readLogLevel)
      ( long "log"
          <> short 'l'
          <> metavar "LOG_LEVEL"
          <> showDefault
          <> value Log.Info
          <> help "Minimum log level to output"
      )

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
  (inputBytes, extraSearchDirs, name) <-
    case opts.input of
      StdInput -> do
        cs <- liftIO BS.getContents
        pure (cs, [], "stdin")
      ExpressionInput cs -> do
        pure (cs, [], "cmd")
      FileInput inPathStr -> do
        cs <- BS.readFile inPathStr
        pure (cs, [Path.takeDirectory inPathStr], (toS inPathStr))
  let searchDirs = opts.searchDirs <> extraSearchDirs
  zonedTime <- Time.getZonedTime

  let run :: Run.BasicApp a -> IO a
      run x = Run.unsafeEval $ Run.evalAppIO searchDirs opts.logLevel zonedTime name inputBytes x

  case opts.mode of
    LexMode -> do
      lts <- run (\e s -> Run.runInputApp e s (pure <$> Run.Lex.lexAll))
      putText $ sformat Run.Lex.fmtLexResult lts
    ExpandMode -> do
      resultList <- run (\e s -> Run.runPTSourceApp e s (pure <$> Run.Expand.expandAll))
      putText $ sformat Run.Expand.fmtExpandResult resultList
    RawCommandMode -> do
      commandList <- run (\e s -> Run.runCommandSourceApp e s (pure <$> Run.Parse.parseAll))
      putText $ sformat Run.Parse.fmtCommandList commandList
    EvalCommandMode -> do
      commandList <- run (\e s -> Run.runEvaluateApp e s (pure <$> Run.Evaluate.evaluateAll))
      putText $ sformat Run.Evaluate.fmtEvalCommandList commandList
    ParaListMode -> do
      (endReason, paraList) <- run (\e s -> Run.runExtractorApp e s (pure <$> Run.Interpret.extractParaHList))
      putText $ "End reason: " <> show endReason
      putText $ sformat fmtHListMultiLine paraList
    VListMode -> do
      vList <- run (\e s -> Run.runExtractorApp e s (pure <$> Run.Interpret.extractMainVList))
      putText $ sformat fmtVListElemSeq vList
    PageMode -> do
      pages <- run (\e s -> Run.runExtractorApp e s (pure <$> Run.Paginate.paginateAll))
      putText $ sformat Run.Paginate.fmtPages pages
    DVIDocInstructionMode -> do
      dviDocInstrs <- run (\e s -> Run.runExtractorApp e s (pure <$> Run.Render.renderToDocInstructions))
      putText $ sformat Render.Doc.fmtDocInstructions dviDocInstrs
    DVISpecInstructionMode -> do
      dviSpecInstrs <- run (\e s -> Run.runExtractorApp e s (pure <$> Run.Render.renderToSpecInstructions))
      putText $ sformat Render.Spec.fmtSpecInstructions dviSpecInstrs
    DVIWriteMode dviOptions -> do
      dviBytes <- run (\e s -> Run.runExtractorApp e s (pure <$> Run.Render.renderToDVIBytes))
      BS.writeFile (dviOptions.dviOutputPath) dviBytes
    _ ->
      putText $ "Unsupported mode: " <> show (opts.mode)

  pure ()
