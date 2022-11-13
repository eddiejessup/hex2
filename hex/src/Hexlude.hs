{-# OPTIONS_GHC -Wno-deprecations #-}

module Hexlude
  ( module Protolude,
    module Optics.Core,
    module Data.Generics.Product,
    module Data.Generics.Sum,
    module Data.Sequence,
    module Data.Group,
    module Data.Sequence.Optics,
    module Formatting,
    module Hexlude.Concept,
    module Hexlude.NonEmptySeq,
    module Effectful,
    traceShowIdM,
    traceResultM,
    notImplemented,
    Fmt,
    (|%|),
    fmtMapWithHeading,
    fmtListWithHeading,
    fmtListOneLine,
    fmtMap,
    fmtViewed,
    flap,
    foldMapM,
    know,
    use,
    modifying,
    assign,
    seqHeadMay,
    seqLastMay,
    indexed,
    indexedNE,
    makeEffect,
    send,
    interpret,
    reinterpret,
    note,
    nothingToError,
    module Effectful.Reader.Dynamic,
    module Effectful.Error.Dynamic,
    module Effectful.NonDet,
    module Effectful.Writer.Dynamic,
    module Effectful.State.Dynamic,
  )
where

import Data.Generics.Product (HasType, field, typed)
import Data.Generics.Sum (AsType, injectTyped, _Ctor, _Typed)
import Data.Group (Group (..), (~~))
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq (Empty, (:<|), (:|>)), singleton, (><))
import Data.Sequence.Optics (seqOf)
import Data.Text.Lazy.Builder qualified as Text.Lazy
import Effectful
import Effectful.Dispatch.Dynamic (interpret, reinterpret, send)
import Effectful.Error.Dynamic
import Effectful.NonDet hiding (Empty)
import Effectful.Reader.Dynamic
import Effectful.State.Dynamic
import Effectful.TH (makeEffect)
import Effectful.Writer.Dynamic
import Formatting (Format, bformat, later, sformat)
import Formatting qualified as F
import GHC.Num qualified as Num
import Hexlude.Concept
import Hexlude.NonEmptySeq (NonEmptySeq)
-- Import `Optics.At` for instance:
--   (Eq k, Hashable k) => At (HashMap k a)
-- So we can do `at` on a HashMap, for control sequence map
import Optics.At ()
import Optics.Core hiding (Empty)
import Protolude hiding
  ( Num (..),
    SrcLoc,
    U1,
    isDigit,
    isLower,
    isSpace,
    isUpper,
    length,
    log,
    notImplemented,
    note,
    to,
    uncons,
    unsnoc,
    words,
    (%),
  )

traceShowIdM :: (Show a, Applicative m) => Text -> a -> m a
traceShowIdM prefix a = pure $ traceShow (prefix <> show a) a

traceResultM :: (Show a, Monad m) => Text -> m a -> m a
traceResultM prefix prog = prog >>= traceShowIdM prefix

notImplemented :: Text -> a
notImplemented msg =
  panic $ "Not implemented: " <> msg

-- Formatting.

(|%|) :: Format r a -> Format r' r -> Format r' a
(|%|) = (F.%)

fmtViewed :: Is k A_Getter => Optic' k is s a -> Format r (a -> r) -> Format r (s -> r)
fmtViewed lens_ = F.accessed (view lens_)

-- Quite a specific function, useful for printing big maps like register-maps,
-- parameters maps etc.
fmtListWithHeading :: Text.Lazy.Builder -> (s -> [v]) -> Fmt v -> Fmt s
fmtListWithHeading title accessor fmtValue =
  fmtWithHeading title accessor (fmtList fmtValue)

-- Quite a specific function, useful for printing big maps like register-maps,
-- parameters maps etc.
fmtMapWithHeading :: Text.Lazy.Builder -> (s -> Map k v) -> Fmt k -> Fmt v -> Fmt s
fmtMapWithHeading title accessor fmtKey fmtValue =
  fmtWithHeading title accessor (fmtMap fmtKey fmtValue)

fmtWithHeading :: Text.Lazy.Builder -> (s -> a) -> Format r (a -> r) -> Format r (s -> r)
fmtWithHeading title accessor fmtContents =
  F.prefixed (title <> ":\n") $ F.reindented 4 (F.accessed accessor fmtContents) |%| "\n"

fmtList :: Fmt v -> Fmt [v]
fmtList fmtValue = F.later $ \case
  [] -> "No entries."
  xs -> F.bformat (F.unlined (F.prefixed "- " fmtValue)) xs

fmtListOneLine :: Fmt v -> Fmt [v]
fmtListOneLine fmtValue = F.later $ \case
  [] -> "No entries."
  xs -> F.bformat (F.commaSpaceSep fmtValue) xs

fmtMap :: forall k v. Fmt k -> Fmt v -> Fmt (Map k v)
fmtMap fmtKey fmtValue = F.accessed Map.toList (fmtList fmtKVEntry)
  where
    fmtKVEntry :: Fmt (k, v)
    fmtKVEntry = F.accessed fst fmtKey |%| ": " <> F.accessed snd fmtValue

type Fmt a = forall r. Format r (a -> r)

-- Stolen from relude.
flap :: Functor f => f (a -> b) -> a -> f b
flap ff x = (\f -> f x) <$> ff
{-# INLINE flap #-}

-- Stolen from relude.
foldMapM :: forall b m f a. (Monoid b, Monad m, Foldable f) => (a -> m b) -> f a -> m b
foldMapM f xs = foldr step return xs mempty
  where
    step x r z = f x >>= \y -> r $! z `mappend` y
{-# INLINE foldMapM #-}

-- Optic for Reader context, by analogy with 'use' in Optics.Extra.
know ::
  (Reader r :> es, Is k A_Getter) =>
  Optic' k is r a ->
  Eff es a
know o = asks (view o)

use ::
  (State s :> es, Is k A_Getter) =>
  Optic' k is s a ->
  Eff es a
use o = gets (view o)

modifying ::
  (State s :> es, Is k A_Setter) =>
  Optic k is s s a b ->
  (a -> b) ->
  Eff es ()
modifying o = modify . over' o

assign ::
  (State s :> es, Is k A_Setter) =>
  Optic k is s s a b ->
  b ->
  Eff es ()
assign o = modifying o . const

seqLastMay :: Seq a -> Maybe a
seqLastMay = \case
  _ :|> x -> Just x
  _ -> Nothing

seqHeadMay :: Seq a -> Maybe a
seqHeadMay = \case
  x :<| _ -> Just x
  _ -> Nothing

-- Taken from package ilist-0.4.0.1.
indexedFrom :: Int -> [b] -> [(Int, b)]
indexedFrom i (a : as) = (i, a) : indexedFrom (i Num.+ 1) as
indexedFrom _ _ = []

indexed :: [a] -> [(Int, a)]
indexed xs = indexedFrom 0 xs

indexedNE :: NonEmpty a -> NonEmpty (Int, a)
indexedNE (a :| as) =
  (0, a) :| indexedFrom 1 as

note :: Error e :> es => e -> Maybe a -> Eff es a
note err = maybe (throwError err) pure

nothingToError :: Error e :> es => e -> Eff es (Maybe a) -> Eff es a
nothingToError e prog = prog >>= note e
