{-# OPTIONS_GHC -Wno-deprecations #-}

module Hexlude
  ( module Protolude,
    module Optics.Core,
    module Optics.State,
    module Data.Generics.Product,
    module Data.Generics.Sum,
    module Data.Sequence,
    module Data.Group,
    module Data.Sequence.Optics,
    module Formatting,
    (|%|),
    (|<>|),
    traceShowIdM,
    traceResultM,
    notImplemented,
    fmtViewed,
    Fmt,
    flap,
    nothingToError,
    know,
  )
where

import Data.Generics.Product (HasType, field, typed)
import Data.Generics.Sum (AsType, injectTyped, _Ctor, _Typed)
import Data.Group (Group (..), (~~))
import Data.Sequence (Seq (Empty, (:<|), (:|>)), singleton, (><))
import Data.Sequence.Optics (seqOf)
-- Import `Optics.At` for instance:
--   (Eq k, Hashable k) => At (HashMap k a)
-- So we can do `at` on a HashMap, for control sequence map

import Formatting (Format, bformat, later, sformat)
import Formatting qualified as F
import Optics.At ()
import Optics.Core hiding (Empty)
import Optics.State (assign', modifying', use)
import Protolude hiding (isDigit, isLower, isSpace, isUpper, notImplemented, to, uncons, unsnoc, words, (%))

traceShowIdM :: (Show a, Applicative m) => Text -> a -> m a
traceShowIdM prefix a = pure $ traceShow (prefix <> show a) a

traceResultM :: (Show a, Monad m) => Text -> m a -> m a
traceResultM prefix prog = prog >>= traceShowIdM prefix

notImplemented :: Text -> a
notImplemented msg =
  panic $ "Not implemented: " <> msg

nothingToError :: MonadError e m => m (Maybe a) -> e -> m a
nothingToError prog eofError = do
  prog >>= \case
    Nothing -> throwError eofError
    Just v -> pure v

(|%|) :: Format r a -> Format r' r -> Format r' a
(|%|) = (F.%)

(|<>|) :: Format r (a -> r) -> Format r (a -> r) -> Format r (a -> r)
(|<>|) = (<>)

fmtViewed :: Is k A_Getter => Optic' k is s a -> Format r (a -> r) -> Format r (s -> r)
fmtViewed lens_ = F.accessed (view lens_)

type Fmt a r = Format r (a -> r)

-- Stolen from relude.
flap :: Functor f => f (a -> b) -> a -> f b
flap ff x = (\f -> f x) <$> ff
{-# INLINE flap #-}

-- Optics for MonadReader contexts, by analogy with 'use' etc in Optics.Extra.

know
  :: (Is k A_Getter, MonadReader e m)
  => Optic' k is e a
  -> m a
know o = asks (view o)
