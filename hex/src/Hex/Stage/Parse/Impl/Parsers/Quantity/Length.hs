module Hex.Stage.Parse.Impl.Parsers.Quantity.Length where

import Control.Monad.Combinators qualified as PC
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Codes (pattern Chr_)
import Hex.Common.Codes qualified as Code
import Hex.Common.Quantity qualified as Q
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hex.Stage.Expand.Interface (PrimTokenSource (..))
import Hex.Stage.Expand.Interface qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Combinators
import Hex.Stage.Parse.Impl.Parsers.Quantity.Number qualified as Par
import Hex.Stage.Parse.Interface.AST.Quantity qualified as AST
import Hexlude

parseLength :: [PrimTokenSource, NonDet, Log.HexLog] :>> es => Eff es AST.Length
parseLength = AST.Length <$> Par.parseSigned parseUnsignedLength

parseUnsignedLength :: [PrimTokenSource, NonDet, Log.HexLog] :>> es => Eff es AST.UnsignedLength
parseUnsignedLength =
  PC.choice
    [ AST.NormalLengthAsULength <$> parseNormalLength,
      Par.tryParse $ AST.CoercedLength . AST.InternalGlueAsLength <$> (anyPrim >>= Par.headToParseInternalGlue)
    ]

parseNormalLength :: [PrimTokenSource, NonDet, Log.HexLog] :>> es => Eff es AST.NormalLength
parseNormalLength =
  PC.choice
    [ AST.LengthSemiConstant <$> parseFactor <*> parseUnit,
      Par.tryParse $ AST.InternalLength <$> (anyPrim >>= Par.headToParseInternalLength)
    ]

-- NOTE: The parser order matters because TeX's grammar is ambiguous: '2.2'
-- could be parsed as an integer constant, '2', followed by '.2'. We break the
-- ambiguity by prioritising the rational constant parser.
-- NOTE: Also we need a `try` on the 'rational' parse, because it will gobble up
-- the next digits before it fails when it doesn't see a ',' or '.'.
-- NOTE: Also we put a 'try' on the normal-int case, just because we start with
-- an 'any-prim' so it's likely to over-consume.
parseFactor :: [PrimTokenSource, NonDet, Log.HexLog] :>> es => Eff es AST.Factor
parseFactor =
  PC.choice
    [ Par.tryParse $ AST.DecimalFractionFactor <$> parseRationalConstant,
      Par.tryParse $ Log.debugLog "Parsing normal-int" >> AST.NormalIntFactor <$> (anyPrim >>= Par.headToParseNormalInt)
    ]

parseRationalConstant :: [PrimTokenSource, NonDet, Log.HexLog] :>> es => Eff es AST.DecimalFraction
parseRationalConstant = do
  Log.debugLog "parseRationalConstant"
  wholeDigits <- PC.many (satisfyCharCatThen PT.Expanding Par.decCharToWord)
  skipSatisfied (satisfyCharCatThen PT.Expanding) $ \cc ->
    let chrCode = cc ^. typed @Code.CharCode
     in (cc ^. typed @Code.CoreCatCode == Code.Other) && (chrCode == Code.Chr_ ',' || chrCode == Code.Chr_ '.')
  fracDigits <- PC.many (satisfyCharCatThen PT.Expanding Par.decCharToWord)
  let v = AST.DecimalFraction {AST.wholeDigits, AST.fracDigits}
  Log.debugLog $ "Successfully parsed rational constant, value: " <> show v
  pure v

parseUnit :: [PrimTokenSource, NonDet, Log.HexLog] :>> es => Eff es AST.Unit
parseUnit =
  PC.choice
    [ (skipOptionalSpaces PT.Expanding) *> (AST.InternalUnit <$> parseInternalUnit),
      (AST.PhysicalUnit <$> parseFrame <*> parsePhysicalUnitLit) <* skipOneOptionalSpace PT.Expanding
    ]
  where
    -- 'try' because we
    parseInternalUnit =
      (parseInternalUnitLit <* skipOneOptionalSpace PT.Expanding)
        <|> ( Par.tryParse $
                anyPrim
                  >>= choiceFlap
                    [ fmap AST.InternalIntUnit <$> Par.headToParseInternalInt,
                      fmap AST.InternalLengthUnit <$> Par.headToParseInternalLength,
                      fmap AST.InternalGlueUnit <$> Par.headToParseInternalGlue
                    ]
            )

    parseInternalUnitLit =
      PC.choice
        [ Par.tryParse $ skipKeyword PT.Expanding [Chr_ 'e', Chr_ 'm'] $> AST.Em,
          skipKeyword PT.Expanding [Chr_ 'e', Chr_ 'x'] $> AST.Ex
        ]

    parseFrame =
      parseOptionalKeyword PT.Expanding [Chr_ 't', Chr_ 'r', Chr_ 'u', Chr_ 'e'] <&> \case
        True -> AST.TrueFrame
        False -> AST.MagnifiedFrame

    -- TODO: Use 'try' because keywords with common prefixes lead the parser
    -- down a blind alley. Could refactor to avoid, but it would be ugly.
    -- Leave as later optimisation.
    parsePhysicalUnitLit =
      PC.choice
        [ Par.tryParse $ skipKeyword PT.Expanding [Chr_ 'p', Chr_ 't'] $> Q.Point,
          skipKeyword PT.Expanding [Chr_ 'p', Chr_ 'c'] $> Q.Pica,
          skipKeyword PT.Expanding [Chr_ 'b', Chr_ 'p'] $> Q.BigPoint,
          skipKeyword PT.Expanding [Chr_ 'd', Chr_ 'd'] $> Q.Didot,
          skipKeyword PT.Expanding [Chr_ 'i', Chr_ 'n'] $> Q.Inch,
          skipKeyword PT.Expanding [Chr_ 'm', Chr_ 'm'] $> Q.Millimetre,
          skipKeyword PT.Expanding [Chr_ 's', Chr_ 'p'] $> Q.ScaledPoint,
          Par.tryParse $ skipKeyword PT.Expanding [Chr_ 'c', Chr_ 'c'] $> Q.Cicero,
          skipKeyword PT.Expanding [Chr_ 'c', Chr_ 'm'] $> Q.Centimetre
        ]
