module Hex.Stage.Parse.Impl.Parsers.Quantity.Length where

import Control.Monad.Combinators qualified as PC
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Codes (pattern Chr_)
import Hex.Common.Codes qualified as Code
import Hex.Common.Parse.Interface (MonadPrimTokenParse (..))
import Hex.Common.Parse.Interface qualified as Par
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Parse.Impl.Parsers.Combinators
import Hex.Stage.Parse.Impl.Parsers.Quantity.Number qualified as Par
import Hex.Stage.Parse.Interface.AST.Quantity qualified as AST
import Hexlude

parseLength :: MonadPrimTokenParse m => m AST.Length
parseLength = AST.Length <$> Par.parseSigned parseUnsignedLength

parseUnsignedLength :: MonadPrimTokenParse m => m AST.UnsignedLength
parseUnsignedLength =
  PC.choice
    [ AST.NormalLengthAsULength <$> parseNormalLength,
      Par.try $ AST.CoercedLength . AST.InternalGlueAsLength <$> (anyPrim >>= Par.headToParseInternalGlue)
    ]

parseNormalLength :: MonadPrimTokenParse m => m AST.NormalLength
parseNormalLength =
  PC.choice
    [ AST.LengthSemiConstant <$> parseFactor <*> parseUnit,
      Par.try $ AST.InternalLength <$> (anyPrim >>= Par.headToParseInternalLength)
    ]

-- NOTE: The parser order matters because TeX's grammar is ambiguous: '2.2'
-- could be parsed as an integer constant, '2', followed by '.2'. We break the
-- ambiguity by prioritising the rational constant parser.
-- NOTE: Also we need a `try` on the 'rational' parse, because it will gobble up
-- the next digits before it fails when it doesn't see a ',' or '.'.
-- NOTE: Also we put a 'try' on the normal-int case, just because we start with
-- an 'any-prim' so it's likely to over-consume.
parseFactor :: MonadPrimTokenParse m => m AST.Factor
parseFactor =
  PC.choice
    [ Par.try $ AST.DecimalFractionFactor <$> parseRationalConstant,
      Par.try $ Log.debugLog "Parsing normal-int" >> AST.NormalIntFactor <$> (anyPrim >>= Par.headToParseNormalInt)
    ]

parseRationalConstant :: MonadPrimTokenParse m => m AST.DecimalFraction
parseRationalConstant = do
  Log.debugLog "parseRationalConstant"
  wholeDigits <- PC.many (satisfyCharCatThen Expanding Par.decCharToWord)
  skipSatisfied (satisfyCharCatThen Expanding) $ \cc ->
    let chrCode = cc ^. typed @Code.CharCode
     in (cc ^. typed @Code.CoreCatCode == Code.Other) && (chrCode == Code.Chr_ ',' || chrCode == Code.Chr_ '.')
  fracDigits <- PC.many (satisfyCharCatThen Expanding Par.decCharToWord)
  let v = AST.DecimalFraction {AST.wholeDigits, AST.fracDigits}
  Log.debugLog $ "Successfully parsed rational constant, value: " <> show v
  pure v

parseUnit :: MonadPrimTokenParse m => m AST.Unit
parseUnit =
  PC.choice
    [ (skipOptionalSpaces Expanding) *> (AST.InternalUnit <$> parseInternalUnit),
      (AST.PhysicalUnit <$> parseFrame <*> parsePhysicalUnitLit) <* skipOneOptionalSpace Expanding
    ]
  where
    -- 'try' because we
    parseInternalUnit =
      (parseInternalUnitLit <* skipOneOptionalSpace Expanding)
        <|> ( Par.try $
                anyPrim
                  >>= choiceFlap
                    [ fmap AST.InternalIntUnit <$> Par.headToParseInternalInt,
                      fmap AST.InternalLengthUnit <$> Par.headToParseInternalLength,
                      fmap AST.InternalGlueUnit <$> Par.headToParseInternalGlue
                    ]
            )

    parseInternalUnitLit =
      PC.choice
        [ Par.try $ skipKeyword Expanding [Chr_ 'e', Chr_ 'm'] $> AST.Em,
          skipKeyword Expanding [Chr_ 'e', Chr_ 'x'] $> AST.Ex
        ]

    parseFrame =
      parseOptionalKeyword Expanding [Chr_ 't', Chr_ 'r', Chr_ 'u', Chr_ 'e'] <&> \case
        True -> AST.TrueFrame
        False -> AST.MagnifiedFrame

    -- TODO: Use 'try' because keywords with common prefixes lead the parser
    -- down a blind alley. Could refactor to avoid, but it would be ugly.
    -- Leave as later optimisation.
    parsePhysicalUnitLit =
      PC.choice
        [ Par.try $ skipKeyword Expanding [Chr_ 'p', Chr_ 't'] $> Q.Point,
          skipKeyword Expanding [Chr_ 'p', Chr_ 'c'] $> Q.Pica,
          skipKeyword Expanding [Chr_ 'b', Chr_ 'p'] $> Q.BigPoint,
          skipKeyword Expanding [Chr_ 'd', Chr_ 'd'] $> Q.Didot,
          skipKeyword Expanding [Chr_ 'i', Chr_ 'n'] $> Q.Inch,
          skipKeyword Expanding [Chr_ 'm', Chr_ 'm'] $> Q.Millimetre,
          skipKeyword Expanding [Chr_ 's', Chr_ 'p'] $> Q.ScaledPoint,
          Par.try $ skipKeyword Expanding [Chr_ 'c', Chr_ 'c'] $> Q.Cicero,
          skipKeyword Expanding [Chr_ 'c', Chr_ 'm'] $> Q.Centimetre
        ]
