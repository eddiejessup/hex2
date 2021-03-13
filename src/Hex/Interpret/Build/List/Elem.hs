module Hex.Interpret.Build.List.Elem where

import Hex.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Quantity qualified as H.Q
import Hexlude

-- Elements.

-- Vertical list.
data VListElem
  = VListBaseElem H.Inter.B.Box.BaseElem
  | ListGlue H.Q.Glue
  | ListPenalty Penalty
  deriving stock (Show, Generic)

-- Horizontal list.
-- TODO: WhatsIt, Leaders, Mark, Insertion
-- TODO: Ligature, DiscretionaryBreak, Math on/off, V-adust
data HListElem
  = HVListElem VListElem
  | HListHBaseElem H.Inter.B.Box.HBaseElem
  deriving stock (Show, Generic)

-- Lists.

newtype HList = HList {unHList :: Seq HListElem}
  deriving stock (Show)
  deriving newtype (Semigroup, Monoid)

newtype VList = VList {unVList :: Seq VListElem}
  deriving stock (Show)
  deriving newtype (Semigroup, Monoid)

-- Element constituents.

data VBoxAlignType
  = DefaultAlign -- \vbox
  | TopAlign -- \vtop
  deriving stock (Show, Eq, Generic)

newtype Penalty = Penalty {unPenalty :: Int}
  deriving stock (Show, Generic)

data DesiredLength = Natural | Spread H.Q.Length | To H.Q.Length
  deriving stock (Show)
