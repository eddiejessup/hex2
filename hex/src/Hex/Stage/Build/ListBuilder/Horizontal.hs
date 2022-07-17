{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Build.ListBuilder.Horizontal where

import Control.Monad.Trans (MonadTrans (..))
import Hex.Capability.Log.Interface (MonadHexLog (..))
import Hex.Common.HexInput.Interface qualified as HIn
import Hex.Common.HexState.Interface (MonadHexState)
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Stage.Build.ListBuilder.Interface
import Hex.Stage.Build.ListElem qualified as ListElem
import Hex.Stage.Evaluate.Interface (MonadEvaluate (..))
import Hex.Stage.Parse.Interface (MonadCommandSource (..))
import Hexlude

newtype HListBuilderT m a = HListBuilderT {unHListBuilderT :: StateT ListElem.HList m a}
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
      MonadHexLog
    )

instance HIn.MonadHexInput m => HIn.MonadHexInput (HListBuilderT m) where
  endCurrentLine = lift HIn.endCurrentLine

  inputIsFinished = lift HIn.inputIsFinished

  getInput = lift HIn.getInput

  putInput = lift . HIn.putInput

  insertLexToken = lift . HIn.insertLexToken

  insertLexTokens = lift . HIn.insertLexTokens

  getNextLexToken = lift HIn.getNextLexToken

  openInputFile x = lift $ HIn.openInputFile x

runHListBuilderT :: ListElem.HList -> HListBuilderT m a -> m (a, ListElem.HList)
runHListBuilderT initHList app = runStateT (unHListBuilderT app) initHList

execHListBuilderT :: Monad m => ListElem.HList -> HListBuilderT m a -> m ListElem.HList
execHListBuilderT initHList app = execStateT (unHListBuilderT app) initHList

instance MonadTrans HListBuilderT where
  lift = HListBuilderT . lift

instance (HSt.MonadHexState m) => MonadHListBuilder (HListBuilderT m) where
  addHListElement = addHListElementImpl

instance (HSt.MonadHexState m) => MonadHexListBuilder (HListBuilderT m) where
  addVListElement e = addHListElementImpl (ListElem.HVListElem e)

addHListElementImpl :: HSt.MonadHexState m => ListElem.HListElem -> HListBuilderT m ()
addHListElementImpl e = do
  hList <- HListBuilderT $ get
  newHList <- lift $ extendHList e hList
  HListBuilderT $ put newHList

-- | This doesn't need to be monadic, but it probably will be at some point, and
-- this keeps things symmetric with the 'vlist' case.
extendHList ::
  Monad m =>
  ListElem.HListElem ->
  ListElem.HList ->
  m ListElem.HList
extendHList e (ListElem.HList accSeq) =
  pure $ ListElem.HList $ accSeq :|> e
