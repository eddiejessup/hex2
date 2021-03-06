module Hex.Interpret.Build.List.Elem where

import Protolude
import Hex.Interpret.Evaluate.Evaluated qualified as H.Inter.Eval
import Hex.Interpret.Build.Box.Elem qualified as H.Inter.B.Box

-- Elements.

-- Vertical list.
data VListElem
  = VListBaseElem H.Inter.B.Box.BaseElem
  | ListGlue (H.Inter.Eval.Glue H.Inter.Eval.Length)
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

newtype HList = HList (Seq HListElem)
  deriving stock (Show)
  deriving newtype (Semigroup, Monoid)

newtype VList = VList (Seq VListElem)
  deriving stock (Show)
  deriving newtype (Semigroup, Monoid)

-- Element constituents.

data VBoxAlignType
  = DefaultAlign -- \vbox
  | TopAlign -- \vtop
  deriving stock (Show, Eq, Generic)

newtype Penalty = Penalty H.Inter.Eval.HexInt
  deriving stock (Show, Generic)

data DesiredLength = Natural | Spread H.Inter.Eval.Length | To H.Inter.Eval.Length
  deriving stock Show
