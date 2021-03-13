module Hexlude
  ( module Protolude,
    module Optics.Core,
    module Optics.State,
    module Data.Generics.Product,
    module Data.Generics.Sum,
    module Data.Sequence,
    module Data.Group,
    joinTexts,
  )
where

import Data.Generics.Product (HasType, field, typed)
import Data.Generics.Sum (AsType, injectTyped, _Typed)
import Data.Group (Group (..), (~~))
import Data.Sequence (Seq (Empty, (:<|), (:|>)), singleton, (><))
import Data.Text qualified as Tx
-- Import `Optics.At` for instance:
--   (Eq k, Hashable k) => At (HashMap k a)
-- So we can do `at` on a HashMap, for control sequence map
import Optics.At ()
import Optics.Core
  ( AffineTraversal',
    Lens',
    Prism',
    Traversal',
    at',
    headOf,
    lens,
    to,
    toListOf,
    traversed,
    view,
    (%),
    (.~),
    (?~),
    (^.),
    (^..),
  )
import Optics.State (assign', modifying', use)
import Protolude hiding (to, (%))

joinTexts :: Text -> [Text] -> Text
joinTexts = Tx.intercalate
