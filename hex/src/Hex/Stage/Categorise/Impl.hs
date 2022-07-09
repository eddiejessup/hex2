{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Categorise.Impl where

-- import Control.Monad.Trans.Maybe (MaybeT (..))
-- import Data.ByteString qualified as BS
-- import Hex.Capability.Log.Interface (MonadHexLog)
-- import Hex.Capability.Log.Interface qualified as Log
-- import Hex.Common.Codes qualified as Code
-- import Hex.Common.HexState.Interface qualified as HSt
-- import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
-- import Hexlude

-- extractCharCatFromHexLine ::
--   (HSt.MonadHexState m) => HexLine -> m (Maybe (RawCharCat, HexLine))
-- extractCharCatFromHexLine (HexLine xs) = runMaybeT $ do
--   (n1, rest1) <- MaybeT $ pure $ BS.uncons xs
--   -- Next two characters must be identical, and have category
--   -- 'Superscript', and triod character mustn't have category 'EndOfLine'.
--   cat1 <- lift $ HSt.getHexCode Code.CCatCodeType (Code.CharCode n1)
--   let normal = (RawCharCat (Code.CharCode n1) cat1, HexLine rest1)
--   case cat1 of
--     Code.CoreCatCode Code.Superscript -> case BS.uncons rest1 of
--       Just (n2, rest2) | n1 == n2 ->
--         case BS.uncons rest2 of
--           Just (n3, rest3) ->
--             lift (HSt.getHexCode Code.CCatCodeType (Code.CharCode n3)) >>= \case
--               Code.EndOfLine ->
--                 pure normal
--               _ -> do
--                 let char3Triod = Code.CharCode $ if n3 < 64 then n3 + 64 else n3 - 64
--                 cat3 <- lift $ HSt.getHexCode Code.CCatCodeType char3Triod
--                 pure (RawCharCat char3Triod cat3, HexLine rest3)
--           Nothing ->
--             pure normal
--       _ ->
--         pure normal
--     _ ->
--       pure normal

-- newtype MonadCharCatSourceT m a = MonadCharCatSourceT {unMonadCharCatSourceT :: m a}
--   deriving newtype
--     ( Functor,
--       Applicative,
--       Monad,
--       MonadIO,
--       MonadState st,
--       MonadError e,
--       HSt.MonadHexState,
--       MonadHexLog
--     )


-- extractCharCatFromCurrentLine ::
--   ( MonadState st m,
--     HSt.MonadHexState m,
--     HasType LoadedCharSource st
--   ) =>
--   m (Maybe RawCharCat)
-- extractCharCatFromCurrentLine =
--   withOpenCharSource $ \openCharSource -> do
--     extractCharCatFromHexLine openCharSource.currentLine >>= \case
--       Nothing ->
--         pure Nothing
--       Just (charCat, restOfLine) -> do
--         assign' (typed @LoadedCharSource) $
--           Cat.LoadedCharSource.OpenLoadedCharSource $
--             openCharSource & #currentLine .~ restOfLine
--         pure $ Just charCat

-- peekCharCatOnCurrentLineImpl ::
--   ( MonadState st m,
--     HSt.MonadHexState m,
--     HasType LoadedCharSource st
--   ) =>
--   m (Maybe RawCharCat)
-- peekCharCatOnCurrentLineImpl =
--   withOpenCharSource $ \openCharSource ->
--     extractCharCatFromHexLine openCharSource.currentLine <&> \case
--       Nothing ->
--         Nothing
--       Just (charCat, _restOfLine) ->
--         Just charCat

-- extractCharCatFromSource ::
--   ( MonadState st m,
--     HasType LoadedCharSource st,
--     HSt.MonadHexState m,
--     Log.MonadHexLog m
--   ) =>
--   m (Maybe RawCharCat)
-- extractCharCatFromSource = do
--   extractCharCatFromCurrentLine >>= \case
--     Nothing -> do
--       endCurrentLineImpl
--       sourceIsFinishedImpl >>= \case
--         True -> pure Nothing
--         False -> extractCharCatFromSource
--     Just res ->
--       pure $ Just res

-- instance
--   ( Monad m,
--     MonadState st (MonadCharCatSourceT m),
--     HasType LoadedCharSource st,
--     HSt.MonadHexState (MonadCharCatSourceT m),
--     Log.MonadHexLog (MonadCharCatSourceT m)
--   ) =>
--   MonadCharCatSource (MonadCharCatSourceT m)
--   where
--   extractCharCat = extractCharCatFromSource

--   peekCharCatOnCurrentLine = peekCharCatOnCurrentLineImpl

--   endCurrentLine = endCurrentLineImpl

--   sourceIsFinished = sourceIsFinishedImpl
