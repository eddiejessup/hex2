module Main where

import Hexlude
import Options.Applicative

import Hex.Categorise qualified as H.Cat
import Hex.Lex qualified as H.Lex

data Input = FileInput FilePath | StdInput

fileInputParser :: Parser Input
fileInputParser = FileInput <$> strOption
  (  long "file"
  <> short 'f'
  <> metavar "FILENAME"
  <> help "Input file")


dviRunParser :: Parser DVIWriteOptions
dviRunParser = DVIWriteOptions
  <$> strOption (  long "file" <> short 'f' <> metavar "FILENAME" <> help "Output file")

stdInputParser :: Parser Input
stdInputParser = flag' StdInput
  (  long "stdin"
  <> help "Read from stdin" )

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

data AppOptions = AppOptions
  { mode :: RunMode,
    input :: Input,
    searchDirs :: [FilePath],
    withAmbles :: Bool
  }

data DVIWriteOptions = DVIWriteOptions
  { dviOutputPath :: FilePath
  }

runModeParser :: Parser RunMode
runModeParser = subparser
    ( command "cat" (info (pure CatMode) (progDesc ""))
   <> command "lex" (info (pure LexMode) (progDesc ""))
   )

appOptionsParser :: Parser AppOptions
appOptionsParser = AppOptions
  <$> runModeParser
  <*> inputParser
  <*> many (strOption (  long "dir" <> short 'd' <> metavar "SEARCH_DIR" <> help "Directory to search for support files"))
  <*> switch (long "amble" <> short 'a' <> help "Surround input with pre- and post-amble")

appOptionsParserInfo :: ParserInfo AppOptions
appOptionsParserInfo = info (appOptionsParser <**> helper)
      ( fullDesc
     <> progDesc "Run Hex source"
     <> header "Hex")

main :: IO ()
main = do
  opts <- execParser appOptionsParserInfo
  (input, _maybeInPath) <-
    case input opts of
      StdInput -> do
        cs <- liftIO BS.getContents
        pure (cs, Nothing)
      FileInput inPathStr -> do
        undefined
        -- path <- Path.IO.resolveFile' (toS inPathStr)
        -- cs <- BS.readFile (Path.toFilePath path)
        -- pure (cs, Just path)

  case mode opts of
    CatMode ->
      putText $ H.Cat.renderCategoriseResult $ H.Cat.usableCharsToCharCats input
    LexMode ->
      putText $ H.Lex.renderLexResult $ H.Lex.usableCharsToLexTokens input
    _ ->
      panic "Not implemented"
