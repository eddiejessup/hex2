module Hexlude.NonEmptySeq where

import Data.Sequence (Seq (..), (<|))
import Data.Sequence qualified as Seq
import Prelude

data NonEmptySeq a = NonEmptySeq a (Seq a)

instance Semigroup (NonEmptySeq a) where
  NonEmptySeq a as <> NonEmptySeq b bs =
    NonEmptySeq a (as <> Seq.singleton b <> bs)

instance Foldable NonEmptySeq where
  foldMap f (NonEmptySeq v vSeq) =
    f v <> foldMap f vSeq

instance Functor NonEmptySeq where
  fmap f (NonEmptySeq v vSeq) =
    NonEmptySeq (f v) (f <$> vSeq)

instance Traversable NonEmptySeq where
  traverse f (NonEmptySeq v vSeq) =
    NonEmptySeq <$> f v <*> (traverse f vSeq)

(|>|) :: a -> Seq a -> NonEmptySeq a
(|>|) = NonEmptySeq

singleton :: a -> NonEmptySeq a
singleton x = x |>| Empty

seqConcat :: Seq a -> NonEmptySeq a -> NonEmptySeq a
seqConcat a b = case nonEmpty a of
  Nothing ->
    b
  Just (NonEmptySeq ax axs) ->
    NonEmptySeq ax (axs <> toSeq b)

nonEmpty :: Seq a -> Maybe (NonEmptySeq a)
nonEmpty = \case
  Empty -> Nothing
  x :<| xs -> Just (NonEmptySeq x xs)

toSeq :: NonEmptySeq a -> Seq a
toSeq (NonEmptySeq x xs) = x <| xs

length :: NonEmptySeq a -> Int
length (NonEmptySeq _ xs) = 1 + Seq.length xs
