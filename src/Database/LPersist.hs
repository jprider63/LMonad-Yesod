module Database.LPersist where

import Control.Monad.Reader (ReaderT)
import Database.Persist (Entity(..),PersistStore,PersistEntity,PersistEntityBackend, Key)
import qualified Database.Persist as Persist
import LMonad
import Yesod.Core

-- | `LEntity` typeclass to taint labels when reading, writing, and creating entity fields.
-- Internally used to raise the current label on database calls. 
-- `mkLabels` automatically generates instances of `LEntity` for your model. 
class Label l => LEntity l e where
    getLabelRead :: LMonad m => Entity e -> LMonadT l m ()
    getLabelWrite :: LMonad m => Entity e -> LMonadT l m ()
    getLabelCreate :: LMonad m => e -> LMonadT l m ()

raiseLabelRead :: (Label l, LMonad m) => Entity e -> LMonadT l m l
raiseLabelWrite :: (Label l, LMonad m) => Entity e -> LMonadT l m l
raiseLabelCreate :: (Label l, LMonad m) => Entity e -> LMonadT l m l

-- | Typeclass for protected entities.
-- `mkLabels` automatically generates these instances.
class Label l => ProtectedEntity l e p | e -> p where
    toProtected :: LMonad m => Entity e -> LMonadT l m p

-- | Persist functions to interact with database. 

get :: (LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => Key v -> ReaderT backend (LMonadT l m) (Maybe v)
get key = do
    res <- Persist.get key
    whenJust res $ lift . raiseLabelRead . (Entity key)
    return res

pGet :: (ProtectedEntity l v p, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => Key v -> ReaderT backend (LMonadT l m) (Maybe p)
pGet key = do
    res <- Persist.get key
    maybe (return Nothing) handler res
    where
        handler val =
            let ent = Entity key val in
            do
            protected <- lift $ toProtected ent
            return $ Just protected

insert :: (LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => v -> ReaderT backend (LMonadT l m) (Key v)
insert val = do
    lift $ raiseLabelCreate val
    Persist.insert val

insert_ :: (LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => v -> ReaderT backend (LMonadT l m) ()
insert_ val = do
    lift $ raiseLabelCreate val
    Persist.insert_ val

insertMany :: (LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => [v] -> ReaderT backend (LMonadT l m) [Key v]
insertMany vals = do
    lift $ mapM_ raiseLabelCreate vals
    Persist.insertMany vals

insertKey :: (LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => (Key v) -> v -> ReaderT backend (LMonadT l m) ()
insertKey key val = do
    lift $ raiseLabelCreate val
    Persist.insertKey key val

repsert :: (LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => (Key v) -> v -> ReaderT backend (LMonadT l m) ()
repsert key val = do
    lift $ raiseLabelCreate val
    res <- Persist.get key
    whenJust res $ lift . raiseLabelWrite . (Entity key)
    Persist.repsert key val

replace :: (LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => (Key v) -> v -> ReaderT backend (LMonadT l m) ()
replace key val = do
    lift $ raiseLabelCreate val
    res <- Persist.get key
    whenJust res $ lift . raiseLabelWrite . (Entity key)
    Persist.replace key val

delete :: (LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => (Key v) -> ReaderT backend (LMonadT l m) ()
delete key = do
    res <- Persist.get key
    whenJust res $ \val -> do
        lift $ raiseLabelWrite $ Entity key val
        Persist.delete key

-- TODO: Make this more precise by using `EntityField`?
-- update :: (LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => (Key v) -> [Update v] -> ReaderT backend (LMonadT l m) ()
-- update key updates = do
--     res <- Persist.get key
--     whenJust res $ \val -> do
        
        



-- | Helper functions.

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust m f = case m of
    Just v ->
        f v
    Nothing ->
        return ()
