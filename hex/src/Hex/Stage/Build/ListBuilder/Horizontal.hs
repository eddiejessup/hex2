{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Build.ListBuilder.Horizontal where

import Control.Monad.Trans (MonadTrans (..))
import Hex.Capability.Log.Interface (MonadHexLog (..))
import Hex.Common.HexState.Interface (MonadHexState)
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Stage.Build.ListBuilder.Interface
import Hex.Stage.Build.ListElem qualified as H.Inter.B.List
import Hex.Stage.Evaluate.Interface (MonadEvaluate (..))
import Hex.Stage.Lex.Interface (MonadLexTokenSource (..))
import Hex.Stage.Parse.Interface (MonadCommandSource (..))
import Hexlude

newtype HListBuilderT m a = HListBuilderT {unHListBuilderT :: StateT H.Inter.B.List.HList m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader r,
      MonadError e,
      MonadHexState,
      MonadCommandSource,
      MonadEvaluate,
      MonadHexLog,
      MonadLexTokenSource
    )

runHListBuilderT :: H.Inter.B.List.HList -> HListBuilderT m a -> m (a, H.Inter.B.List.HList)
runHListBuilderT initHList app = runStateT (unHListBuilderT app) initHList

execHListBuilderT :: Monad m => H.Inter.B.List.HList -> HListBuilderT m a -> m H.Inter.B.List.HList
execHListBuilderT initHList app = execStateT (unHListBuilderT app) initHList

instance MonadTrans HListBuilderT where
  lift = HListBuilderT . lift

instance (HSt.MonadHexState m) => MonadHListBuilder (HListBuilderT m) where
  addHListElement = addHListElementImpl

instance (HSt.MonadHexState m) => MonadHexListBuilder (HListBuilderT m) where
  addVListElement e = addHListElementImpl (H.Inter.B.List.HVListElem e)

addHListElementImpl :: HSt.MonadHexState m => H.Inter.B.List.HListElem -> HListBuilderT m ()
addHListElementImpl e = do
  hList <- HListBuilderT $ get
  newHList <- lift $ extendHList e hList
  HListBuilderT $ put newHList

-- | This doesn't need to be monadic, but it probably will be at some point, and
-- this keeps things symmetric with the 'vlist' case.
extendHList ::
  Monad m =>
  H.Inter.B.List.HListElem ->
  H.Inter.B.List.HList ->
  m H.Inter.B.List.HList
extendHList e (H.Inter.B.List.HList accSeq) =
  pure $ H.Inter.B.List.HList $ accSeq :|> e
