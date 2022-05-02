{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Hex.Stage.Evaluate.Impl where

import Hexlude
import Hex.Stage.Evaluate.Interface (MonadEvaluate (..))
import qualified Hex.Stage.Parse.Interface.AST.Command as UnevalAST
import qualified Hex.Stage.Evaluate.Interface.AST.Command as EvalAST
import qualified Hex.Stage.Evaluate.Impl.Eval as Eval

instance Monad m => MonadEvaluate m where
  evalCommand :: UnevalAST.Command -> m EvalAST.Command
  evalCommand = \case
    UnevalAST.ShowToken lt -> pure $ EvalAST.ShowToken lt
    UnevalAST.ShowBox n -> EvalAST.ShowBox <$> Eval.evalInt n
    -- UnevalAST.ShowLists
    -- UnevalAST.ShowTheInternalQuantity InternalQuantity
    -- UnevalAST.ShipOut Box
    -- UnevalAST.AddMark H.Sym.Tok.Syn.ExpandedBalancedText
    -- -- -- Note: this *is* an all-modes command. It can happen in non-vertical modes,
    --   -- -- then can 'migrate' out.
    --   -- \AddInsertion HexInt VModeMaterial
    --   -- \AddAdjustment VModeMaterial
    --   AddSpace
    -- StartParagraph H.Sym.Tok.IndentFlag
    -- EndParagraph
    -- -- \AddAlignedMaterial DesiredLength AlignmentMaterial
    --   HModeCommand HModeCommand
    -- VModeCommand VModeCommand
    -- ModeIndependentCommand ModeIndependentCommand
