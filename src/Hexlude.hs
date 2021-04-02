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
    fmtViewed,
    Fmt,
    flap,
  )
where

import Data.Generics.Product (HasType, field, typed)
import Data.Generics.Sum (AsType, injectTyped, _Typed, _Ctor)
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
import Protolude hiding (to, uncons, unsnoc, (%), isSpace, isDigit, words)

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
