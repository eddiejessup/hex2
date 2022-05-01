module Main where

import Data.ByteString qualified as BS
import Formatting qualified as F
import Hex.Run.App qualified as Run
import Hex.Run.Categorise qualified as Run.Cat
import Hex.Run.Lex qualified as Run.Lex
import Hex.Run.Resolve qualified as Run.Resolve
import Hexlude
import Options.Applicative
import Hex.Stage.Resolve.Interface (ResolutionMode(..))

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
  | ResolveMode
  | ExpandMode
  | CommandMode
  | ParaListMode
  | ParaSetMode
  | PageListMode
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
    ResolveMode -> do
      resultList <- Run.unsafeEvalApp input (Run.Resolve.resolveAll Resolving)
      putText $ sformat Run.Resolve.fmtResolveResult resultList
    _ ->
      putText $ "Unsupported mode: " <> show (opts.mode)
  pure ()