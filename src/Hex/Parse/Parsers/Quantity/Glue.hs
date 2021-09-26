{-# LANGUAGE PatternSynonyms #-}

module Hex.Parse.Parsers.Quantity.Glue where

import Control.Monad.Combinators qualified as PC
import Hex.Codes (pattern Chr_)
import Hex.Codes qualified as H.C
import Hex.Parse.Syntax.Quantity qualified as Syn
import Hex.Parse.MonadPrimTokenSource.Interface
import Hex.Parse.Parsers.Combinators qualified as Par
import Hex.Parse.Parsers.Quantity.Length qualified as Par
import Hex.Parse.Parsers.Quantity.Number qualified as Par
import Hex.Quantity qualified as H.Q
import Hex.Symbol.Token.Primitive qualified as T
import Hexlude

headToParseModedAddGlue :: MonadPrimTokenSource m => H.Q.Axis -> T.PrimitiveToken -> m Syn.Glue
headToParseModedAddGlue axis = \case
  T.ModedCommand tokenAxis modedTok | tokenAxis == axis ->
    case modedTok of
      -- \hskip 10pt and such.
      T.SpecifiedGlueTok ->
        parseGlue
      T.PresetGlueTok presetTok ->
        pure $
          Syn.ExplicitGlue $ case presetTok of
            -- \{v,h}fil:    0pt plus 1fil
            T.Fil ->
              noLengthGlueSpec (Just Syn.oneFilFlex) Nothing
            -- \{v,h}fill:   0pt plus 1fill
            T.Fill ->
              noLengthGlueSpec (Just Syn.oneFillFlex) Nothing
            -- \{v,h}ss:     0pt plus 1fil minus 1fil
            T.StretchOrShrink ->
              noLengthGlueSpec (Just Syn.oneFilFlex) (Just Syn.oneFilFlex)
            -- \{v,h}filneg: 0pt plus -1fil
            T.FilNeg ->
              noLengthGlueSpec (Just Syn.minusOneFilFlex) Nothing
      _ ->
        empty
  _ ->
    empty
  where
    noLengthGlueSpec = Syn.ExplicitGlueSpec Syn.zeroLength

parseGlue :: MonadPrimTokenSource m => m Syn.Glue
parseGlue =
  PC.choice
    [ Syn.ExplicitGlue <$> parseExplicitGlueSpec,
      Syn.InternalGlue <$> Par.parseSigned (Par.parseHeaded Par.headToParseInternalGlue)
    ]

parseExplicitGlueSpec :: MonadPrimTokenSource m => m Syn.ExplicitGlueSpec
parseExplicitGlueSpec = do
  len <- Par.parseLength
  stretch <- parseFlex [Chr_ 'p', Chr_ 'l', Chr_ 'u', Chr_ 's']
  shrink <- parseFlex [Chr_ 'm', Chr_ 'i', Chr_ 'n', Chr_ 'u', Chr_ 's']
  pure $ Syn.ExplicitGlueSpec len stretch shrink

parseFlex :: MonadPrimTokenSource m => [H.C.CharCode] -> m (Maybe Syn.Flex)
parseFlex s =
  PC.choice
    [ Just <$> parsePresentFlex,
      Par.skipOptionalSpaces $> Nothing
    ]
  where
    parsePresentFlex = do
      Par.skipKeyword s
      PC.choice
        [ Syn.FiniteFlex <$> Par.parseLength,
          Syn.FilFlex <$> parseFilLength
        ]

parseFilLength :: MonadPrimTokenSource m => m Syn.FilLength
parseFilLength = do
  factor <- Par.parseSigned Par.parseFactor
  Par.skipKeyword [Chr_ 'f', Chr_ 'i']
  order <-
    parseNrLs >>= \case
      0 -> panic "impossible"
      1 -> pure H.Q.Fil1
      2 -> pure H.Q.Fil2
      3 -> pure H.Q.Fil3
      _ -> empty
  Par.skipOptionalSpaces
  pure $ Syn.FilLength factor order
  where
    parseNrLs = length <$> PC.some (Par.skipSatisfied $ Par.matchNonActiveCharacterUncased (Chr_ 'l'))
