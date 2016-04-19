-- Copyright (c) 2012 Michael Snoyman, http://www.yesodweb.com/
--
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
-- LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
-- OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
-- WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
--
-- Modified by James Parker in 2014. 

{-# LANGUAGE TypeFamilies #-}

module Database.LPersist (
      LEntity(..)
    , raiseLabelRead
    , raiseLabelWrite
    , raiseLabelCreate
    , YesodLPersist (..)
    , lDefaultRunDB
    , ProtectedEntity(..)
    , Protected
    , PEntity(..)
    , get
    , pGet
    , insert
    , insert_
    , insertMany
    , insertKey
    , repsert
    , replace
    , delete
    , update
    , updateGet
    , pUpdateGet
    , getJust
    , pGetJust
    , getBy
    , pGetBy
    , deleteBy
    , insertUnique
    , updateWhere
    , deleteWhere
    , selectFirst
    , pSelectFirst
    , count
    , selectList
    , pSelectList
    , selectKeysList
    ) where

import Control.Exception.Lifted (throwIO)
import Control.Monad
import Control.Monad.Reader (ReaderT)
import Database.Persist (Entity(..),PersistStore,PersistEntity,PersistEntityBackend, Key, Update, Unique, PersistUnique, SelectOpt, Filter, PersistQuery)
import qualified Database.Persist as Persist
import Database.Persist.Sql (SqlBackend, PersistConfig, PersistConfigPool, PersistConfigBackend)
import qualified Database.Persist.Sql as Persist
import qualified Data.Text as Text
import LMonad
import Yesod.Core
import Yesod.Persist (YesodPersist(..))

-- | `LEntity` typeclass to taint labels when reading, writing, and creating entity fields.
-- Internally used to raise the current label on database calls. 
-- `mkLabels` automatically generates instances of `LEntity` for your model. 
class Label l => LEntity l e where
    getLabelRead :: Entity e -> l
    getLabelWrite :: Entity e -> l
    getLabelCreate :: e -> l

raiseLabelRead :: (Label l, LMonad m, LEntity l e) => Entity e -> LMonadT l m ()
raiseLabelRead e = taintLabel $ getLabelRead e

raiseLabelWrite :: (Label l, LMonad m, LEntity l e) => Entity e -> LMonadT l m ()
raiseLabelWrite e = taintLabel $ getLabelWrite e

raiseLabelCreate :: (Label l, LMonad m, LEntity l e) => e -> LMonadT l m ()
raiseLabelCreate e = taintLabel $ getLabelCreate e

-- | Typeclass for protected entities.
-- `mkLabels` automatically generates these instances.
type family Protected e
class Label l => ProtectedEntity l e where
    toProtected :: LMonad m => Entity e -> LMonadT l m (Protected e)

-- | ADT wrapper for protected entities. Analagous to Entity.
data PEntity l e = (ProtectedEntity l e) => PEntity (Key e) (Protected e)

-- | How to run database functions.

class YesodPersist site => YesodLPersist site where
    runDB :: (Label l, m ~ HandlerT site IO) => ReaderT (YesodPersistBackend site) (LMonadT l m) a -> LMonadT l m a

lDefaultRunDB :: (Label l, PersistConfig c, LMonad m, m ~ HandlerT site IO) => (site -> c)
                      -> (site -> PersistConfigPool c)
                      -> PersistConfigBackend c (LMonadT l m) b
                      -> LMonadT l m b
lDefaultRunDB getConfig getPool f = do
    master <- lLift getYesod
    Persist.runPool
        (getConfig master)
        f
        (getPool master)

-- | Persist functions to interact with database. 

get :: (LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => Key v -> ReaderT backend (LMonadT l m) (Maybe v)
get key = do
    res <- Persist.get key
    whenJust res $ lift . raiseLabelRead . (Entity key)
    return res

pGet :: (ProtectedEntity l v, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => Key v -> ReaderT backend (LMonadT l m) (Maybe (Protected v))
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

-- TODO: 
--  Double check this!!
-- | This function only works for SqlBackends since we need to be able to rollback transactions.
update :: (backend ~ SqlBackend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => (Key v) -> [Update v] -> ReaderT backend (LMonadT l m) ()
update = updateHelper (return ()) $ \_ -> return ()

updateGet :: (backend ~ SqlBackend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => (Key v) -> [Update v] -> ReaderT backend (LMonadT l m) v
updateGet key = updateHelper err return key
    where
        err = liftIO $ throwIO $ Persist.KeyNotFound $ Prelude.show key

pUpdateGet :: (backend ~ SqlBackend, ProtectedEntity l v, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => (Key v) -> [Update v] -> ReaderT backend (LMonadT l m) (Protected v)
pUpdateGet key = updateHelper err (toProtected . (Entity key)) key
    where
        err = liftIO $ throwIO $ Persist.KeyNotFound $ Prelude.show key

getJust :: (LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => (Key v) -> ReaderT backend (LMonadT l m) v
getJust key = get key >>= maybe err return
    where
        err = liftIO $ throwIO $ Persist.PersistForeignConstraintUnmet $ Text.pack $ Prelude.show key

pGetJust :: (LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v, ProtectedEntity l v) => (Key v) -> ReaderT backend (LMonadT l m) (Protected v)
pGetJust key = pGet key >>= maybe err return
    where
        err = liftIO $ throwIO $ Persist.PersistForeignConstraintUnmet $ Text.pack $ Prelude.show key

-- TODO
--
-- belongsTo
-- belongsToJust

getBy :: (PersistUnique backend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => Unique v -> ReaderT backend (LMonadT l m) (Maybe (Entity v))
getBy uniq = do
    res <- Persist.getBy uniq
    whenJust res $ lift . raiseLabelRead
    return res

pGetBy :: (ProtectedEntity l v, PersistUnique backend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => Unique v -> ReaderT backend (LMonadT l m) (Maybe (PEntity l v))
pGetBy uniq = do
    res <- Persist.getBy uniq
    lift $ mapM toProtectedWithKey res
    --maybe (return Nothing) (\(Entity key ent) -> lift . return . Just . (PEntity key) =<< toProtected ent) res
    -- maybe (return Nothing) (\ent -> do
    --     pEnt <- lift $ toProtected ent
    --     return $ Just $ PEntity (entityKey ent) pEnt
    --   ) res

deleteBy :: (ProtectedEntity l v, PersistUnique backend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => Unique v -> ReaderT backend (LMonadT l m) ()
deleteBy uniq = do
    res <- Persist.getBy uniq
    whenJust res $ \e -> do
        lift $ raiseLabelWrite e
        Persist.deleteBy uniq

insertUnique :: (ProtectedEntity l v, PersistUnique backend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => v -> ReaderT backend (LMonadT l m) (Maybe (Key v))
insertUnique val = do
    lift $ raiseLabelCreate val
    Persist.insertUnique val

-- TODO
--  upsert
--  getByValue
--  insertBy
--  replaceUnique
--  checkUnique
--  onlyUnique
--
--  selectSourceRes
--  selectKeysRes

updateWhere :: (backend ~ SqlBackend, PersistQuery backend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => [Filter v] -> [Update v] -> ReaderT backend (LMonadT l m) ()
updateWhere filts upts = do
    res <- Persist.selectList filts []
    -- `updateGet` should rollback transaction if any checks fail
    mapM_ (\e -> 
        let k = entityKey e in 
        (lift $ raiseLabelWrite e) >> 
            (updateGet (entityKey e) upts) >>=
                (lift . raiseLabelWrite . (Entity k))
      ) res

deleteWhere :: (PersistQuery backend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => [Filter v] -> ReaderT backend (LMonadT l m) ()
deleteWhere filts = do
    res <- Persist.selectList filts []
    lift $ mapM_ raiseLabelWrite res
    Persist.deleteWhere filts

selectFirst :: (PersistQuery backend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => [Filter v] -> [SelectOpt v] -> ReaderT backend (LMonadT l m) (Maybe (Entity v))
selectFirst filts opts = do
    res <- Persist.selectFirst filts opts
    whenJust res $ lift . raiseLabelRead
    return res

toProtectedWithKey :: (LMonad m, ProtectedEntity l e) => Entity e -> LMonadT l m (PEntity l e)
toProtectedWithKey r = do
    p <- toProtected r
    return $ PEntity (entityKey r) p

pSelectFirst :: (PersistQuery backend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v, ProtectedEntity l v) => [Filter v] -> [SelectOpt v] -> ReaderT backend (LMonadT l m) (Maybe (PEntity l v))
pSelectFirst filts opts = do
    res <- Persist.selectFirst filts opts
    lift $ mapM toProtectedWithKey res
    -- lift $ maybe (return Nothing) (fmap Just . toProtected) res

count :: (PersistQuery backend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => [Filter v] -> ReaderT backend (LMonadT l m) Int
count filts = do
    res <- Persist.selectList filts []
    lift $ foldM (\acc e -> (raiseLabelWrite e) >> (return $ acc + 1)) 0 res

-- TODO
--  selectSource
--  selectKeys

selectList :: (PersistQuery backend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => [Filter v] -> [SelectOpt v] -> ReaderT backend (LMonadT l m) [Entity v]
selectList filts opts = do
    l <- Persist.selectList filts opts
    lift $ mapM_ raiseLabelRead l
    return l

pSelectList :: (PersistQuery backend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v, ProtectedEntity l v) => [Filter v] -> [SelectOpt v] -> ReaderT backend (LMonadT l m) [PEntity l v]
pSelectList filts opts = do
    l <- Persist.selectList filts opts
    lift $ mapM toProtectedWithKey l 

selectKeysList :: (PersistQuery backend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => [Filter v] -> [SelectOpt v] -> ReaderT backend (LMonadT l m) [Key v]
selectKeysList filts opts = do
    l <- Persist.selectList filts opts
    lift $ mapM_ raiseLabelRead l
    return $ map entityKey l
    
-- TODO
--  deleteCascade
--  deleteCascadeWhere



-- | Helper functions.

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust m f = case m of
    Just v ->
        f v
    Nothing ->
        return ()

updateHelper :: (backend ~ SqlBackend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => (LMonadT l m a) -> (v -> LMonadT l m a) -> (Key v) -> [Update v] -> ReaderT backend (LMonadT l m) a
updateHelper n j key updates = do
    res <- Persist.get key
    maybe (lift n) (\oldVal -> do
        lift $ raiseLabelWrite $ Entity key oldVal
        newVal <- Persist.updateGet key updates
        let newL = getLabelWrite $ Entity key newVal
        l <- lift $ lubCurrentLabel newL
        guard <- lift $ canSetLabel l
        unless guard
            -- Rollback
            Persist.transactionUndo
        lift $ setLabel l
        lift $ j newVal
      ) res
