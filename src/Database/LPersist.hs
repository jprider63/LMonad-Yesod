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

{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}

module Database.LPersist (
      LEntity(..)
--    , raiseLabelRead
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
--    , repsert
    , replace
    , delete
    , update
    , updateGet
    , pUpdateGet
    , getJust
    , pGetJust
    , getBy
    , pGetBy
--    , deleteBy
--    , insertUnique
--    , updateWhere
--    , deleteWhere
    , selectFirst
    , pSelectFirst
    , count
    , selectList
    , pSelectList
--    , selectKeysList
    ) where

import Control.Exception.Lifted (throwIO)
import Control.Monad
import Control.Monad.Reader (ReaderT)
import Data.Proxy
import Database.Persist (Entity(..),EntityField(..),PersistStore,PersistEntity,PersistEntityBackend, Key, Update(..), Unique, PersistUnique, SelectOpt, Filter(..), PersistQuery, SelectOpt(..))
import qualified Database.Persist as Persist
import Database.Persist.Sql (SqlBackend, PersistConfig, PersistConfigPool, PersistConfigBackend)
import qualified Database.Persist.Sql as Persist
import qualified Data.Text as Text
import LMonad.TCB
import Yesod.Core
import Yesod.Persist (YesodPersist(..))

import Internal

-- | `LEntity` typeclass to taint labels when reading, writing, and creating entity fields.
-- Internally used to raise the current label on database calls. 
-- `mkLabels` automatically generates instances of `LEntity` for your model. 
class (Label l) => LEntity l e where
    getReadLabels :: Entity e -> [l]
    getWriteLabels :: Entity e -> [l]
    -- getCreateLabels :: e -> [l]

    tableReadLabel :: Proxy e -> l
    tableInsertLabel :: e -> l

-- class Label l => LEntityField l e t where
--     fieldReadLabel :: EntityField e t -> Entity e -> l

class (Label l, PersistEntity e) => LPersistEntity l e | e -> l where
    lPersistFieldLabel :: EntityField e t -> Entity e -> l
    lPersistReadLabelDependencies :: EntityField e t -> [EntityField e v]

-- class Label l => LEntityField f where
--     type LEntityFieldLabel f = l | f -> l
--     type LEntityFieldEntity f = e | f -> e
--     fieldReadLabel :: f -> LEntityFieldEntity f -> LEntityFieldLabel f

-- data FieldToLabel l e = FieldToLabel {
--     unFieldToLabel :: forall t . LEntityField l e t => EntityField e t
--   }

-- instance LTableLength l v => LTableLength l (Entity v) where
--     tableReadLabel Proxy = tableReadLabel (Proxy :: Proxy v)
-- 
-- instance LTableLength l v => LTableLength l (Key v) where
--     tableReadLabel Proxy = tableReadLabel (Proxy :: Proxy v)
-- 
-- instance LTableLength l v => LTableLength l [v] where
--     tableReadLabel Proxy = tableReadLabel (Proxy :: Proxy v)
-- 
-- instance LTableLength l v => LTableLength l (Unique v) where
--     tableReadLabel Proxy = tableReadLabel (Proxy :: Proxy v)

-- filterToFields :: forall l e t . (Label l, LEntityField l e t) => Filter e -> [FieldToLabel l e]
filterToFields :: forall l e . (LPersistEntity l e) => Filter e -> [Entity e -> l]
-- filterToFields (Filter field _ _) = [persistFieldDef (field :: EntityField e t)]
filterToFields (Filter field _ _) = [lPersistFieldLabel field]
filterToFields (FilterAnd fs) = concatMap filterToFields fs
filterToFields (FilterOr fs) = concatMap filterToFields fs
filterToFields (BackendFilter _) = error "filterToFields: Unsupported backend specific filter."

selectOptToFields :: forall l e . (LPersistEntity l e) => SelectOpt e -> [Entity e -> l]
selectOptToFields (Asc field) = [lPersistFieldLabel field]
selectOptToFields (Desc field) = [lPersistFieldLabel field]
selectOptToFields (OffsetBy _) = []
selectOptToFields (LimitTo _) = []

updateToFields :: forall l e . (LPersistEntity l e) => Update e -> [Entity e -> l]
updateToFields (Update field _ _) = map lPersistFieldLabel $ lPersistReadLabelDependencies field
updateToFields (BackendUpdate _) = error "updateToFields: Unsupported backend specific update."

getLabelRead :: LEntity l e => Entity e -> l
getLabelRead = joinLabels . getReadLabels

-- raiseLabelRead :: (Label l, LMonad m, LEntity l e) => Entity e -> LMonadT l m ()
-- raiseLabelRead e = taintLabel $ getLabelRead e

-- | Typeclass for protected entities.
-- `mkLabels` automatically generates these instances.
type family Protected e
class Label l => ProtectedEntity l e | e -> l where
    -- toProtected :: LMonad m => Entity e -> LMonadT l m (Protected e)
    toProtected :: Entity e -> Protected e

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
    master <- LMonadT $ lift getYesod
    Persist.runPool
        (getConfig master)
        f
        (getPool master)

-- | Persist functions to interact with database. 

get :: forall l m v backend . (LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => Key v -> ReaderT backend (LMonadT l m) (Maybe v)
get key = do
    let tRead = tableReadLabel (Proxy :: Proxy v)
    res <- Persist.get key
    lift $ taintLabel $ maybe 
        tRead 
        (\v -> tRead `lub` (getLabelRead $ Entity key v)) res
    return res

pGet :: forall l m v backend . (ProtectedEntity l v, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => Key v -> ReaderT backend (LMonadT l m) (Maybe (Protected v))
pGet key = do
    lift $ taintLabel $ tableReadLabel (Proxy :: Proxy v)
    res <- Persist.get key
    return $ fmap (toProtected . Entity key) res

insert :: forall l m v backend . (LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v, backend ~ SqlBackend) => v -> ReaderT backend (LMonadT l m) (Key v)
insert val = do
    c <- lift getClearance
    lift $ guardCanFlowTo (tableInsertLabel val) c
    l <- lift getCurrentLabel
    lift $ guardCanFlowTo l $ tableReadLabel (Proxy :: Proxy v)
    k <- Persist.insert val
    let e = Entity k val
    mapM_ (guardCanFlowToRollback l) $ getReadLabels e
    return k

insert_ :: (LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v, backend ~ SqlBackend) => v -> ReaderT backend (LMonadT l m) ()
insert_ val = do
    _ <- insert val
    return ()

insertMany :: (LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v, backend ~ SqlBackend) => [v] -> ReaderT backend (LMonadT l m) [Key v]
insertMany vals = mapM insert vals
-- JP: There are some redundant checks we can get rid of.

insertKey :: forall l m v backend . (LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => (Key v) -> v -> ReaderT backend (LMonadT l m) ()
insertKey key val = do
    c <- lift getClearance
    lift $ guardCanFlowTo (tableInsertLabel val) c
    l <- lift getCurrentLabel
    lift $ guardCanFlowTo l $ tableReadLabel (Proxy :: Proxy v)
    lift $ mapM_ (guardCanFlowTo l) $ getReadLabels $ Entity key val
    Persist.insertKey key val

-- repsert :: (LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => (Key v) -> v -> ReaderT backend (LMonadT l m) ()
-- repsert key val = do
--     lift $ raiseLabelCreate val
--     res <- Persist.get key
--     whenJust res $ lift . raiseLabelWrite . (Entity key)
--     Persist.repsert key val

replace :: (LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => (Key v) -> v -> ReaderT backend (LMonadT l m) ()
replace key val = do
    let e = Entity key val
    l <- lift getCurrentLabel
    lift $ mapM_ (guardCanFlowTo l) $ getReadLabels e
    c <- lift getClearance
    lift $ mapM_ (\x -> guardCanFlowTo x c) $ getWriteLabels e
    oldM <- Persist.get key
    whenJust oldM $ \old -> do
        lift $ mapM_ (\x -> guardCanFlowTo x c) $ getWriteLabels $ Entity key old
        Persist.replace key val

delete :: forall l m v backend . (LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => (Key v) -> ReaderT backend (LMonadT l m) ()
delete key = do
    l <- lift getCurrentLabel
    lift $ guardCanFlowTo l $ tableReadLabel (Proxy :: Proxy v)
    res <- Persist.get key
    whenJust res $ \val -> do
        c <- lift getClearance
        lift $ mapM_ (\x -> guardCanFlowTo x c) $ getWriteLabels $ Entity key val
        Persist.delete key

-- | This function only works for SqlBackends since we need to be able to rollback transactions.
update :: (backend ~ SqlBackend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, LPersistEntity l v) => (Key v) -> [Update v] -> ReaderT backend (LMonadT l m) ()
update = updateHelper (return ()) $ const $ return ()


updateGet :: forall backend l m v . (backend ~ SqlBackend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, LPersistEntity l v) => (Key v) -> [Update v] -> ReaderT backend (LMonadT l m) v
updateGet key = updateHelper err f key
    where
        f e = do
            taintLabel $ tRead `lub` (getLabelRead $ Entity key e)
            return e

        tRead = tableReadLabel (Proxy :: Proxy v)
        err = do
            taintLabel tRead
            liftIO $ throwIO $ Persist.KeyNotFound $ Prelude.show key

pUpdateGet :: forall l m v backend . (backend ~ SqlBackend, ProtectedEntity l v, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, LPersistEntity l v) => (Key v) -> [Update v] -> ReaderT backend (LMonadT l m) (Protected v)
pUpdateGet key updates = do
    lift $ taintLabel $ tableReadLabel (Proxy :: Proxy v)
    updateHelper err (return . toProtected . (Entity key)) key updates

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

-- -- TODO
-- --
-- -- belongsTo
-- -- belongsToJust

getBy :: forall l m v backend . (PersistUnique backend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => Unique v -> ReaderT backend (LMonadT l m) (Maybe (Entity v))
getBy uniq = do
    let tRead = tableReadLabel (Proxy :: Proxy v)
    res <- Persist.getBy uniq
    lift $ taintLabel $ maybe tRead (\e -> tRead `lub` getLabelRead e) res
    return res

pGetBy :: (ProtectedEntity l v, PersistUnique backend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => Unique v -> ReaderT backend (LMonadT l m) (Maybe (PEntity l v))
pGetBy uniq = undefined
-- TODO XXX
-- do
--     res <- Persist.getBy uniq
--     lift $ mapM toProtectedWithKey res

-- deleteBy :: (ProtectedEntity l v, PersistUnique backend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => Unique v -> ReaderT backend (LMonadT l m) ()
-- deleteBy uniq = do
--     res <- Persist.getBy uniq
--     whenJust res $ \e -> do
--         lift $ raiseLabelWrite e
--         Persist.deleteBy uniq
-- 
-- insertUnique :: (ProtectedEntity l v, PersistUnique backend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => v -> ReaderT backend (LMonadT l m) (Maybe (Key v))
-- insertUnique val = do
--     lift $ raiseLabelCreate val
--     Persist.insertUnique val
-- 
-- -- TODO
-- --  upsert
-- --  getByValue
-- --  insertBy
-- --  replaceUnique
-- --  checkUnique
-- --  onlyUnique
-- --
-- --  selectSourceRes
-- --  selectKeysRes
-- 
-- updateWhere :: (backend ~ SqlBackend, PersistQuery backend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => [Filter v] -> [Update v] -> ReaderT backend (LMonadT l m) ()
-- updateWhere filts upts = do
--     res <- Persist.selectList filts []
--     -- `updateGet` should rollback transaction if any checks fail
--     mapM_ (\e -> 
--         let k = entityKey e in 
--         (lift $ raiseLabelWrite e) >> 
--             (updateGet (entityKey e) upts) >>=
--                 (lift . raiseLabelWrite . (Entity k))
--       ) res
-- 
-- deleteWhere :: (PersistQuery backend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => [Filter v] -> ReaderT backend (LMonadT l m) ()
-- deleteWhere filts = do
--     res <- Persist.selectList filts []
--     lift $ mapM_ raiseLabelWrite res
--     Persist.deleteWhere filts

selectFirst :: forall l m v backend . (PersistQuery backend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => [Filter v] -> [SelectOpt v] -> ReaderT backend (LMonadT l m) (Maybe (Entity v))
selectFirst filts opts = do
    resM <- Persist.selectFirst filts opts
    let tableL = tableReadLabel (Proxy :: Proxy v)
    lift $ case resM of
        Nothing -> do
            taintLabel tableL
            return resM
        Just res -> do
            taintLabel $ tableL `lub` (getLabelRead res)
            return resM

toProtectedWithKey :: (ProtectedEntity l e) => Entity e -> PEntity l e
toProtectedWithKey r = 
    let p = toProtected r in
    PEntity (entityKey r) p

pSelectFirst :: forall backend l m v . (PersistQuery backend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, LPersistEntity l v, ProtectedEntity l v) => [Filter v] -> [SelectOpt v] -> ReaderT backend (LMonadT l m) (Maybe (PEntity l v))
pSelectFirst filts opts = do
    let tableL = tableReadLabel (Proxy :: Proxy v)

    resM <- Persist.selectFirst filts opts
    lift $ case resM of
        Nothing -> do
            taintLabel tableL

            return Nothing
        Just e -> do
            let lfs = concatMap filterToFields filts
            let los = concatMap selectOptToFields opts
            taintLabel $ lub tableL $ joinLabels $ map ($ e) los ++ map ($ e) lfs

            return $ Just $ toProtectedWithKey e

count :: forall l m v backend . (PersistQuery backend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, LPersistEntity l v) => [Filter v] -> ReaderT backend (LMonadT l m) Int
count filts = do
    res <- Persist.selectList filts []

    let tableL = tableReadLabel (Proxy :: Proxy v)
    let lfs = concatMap filterToFields filts
    lift $ taintLabel $ lub tableL $ joinLabels $ concatMap (\e -> map ($ e) lfs) res

    return $ length res

    -- let (l, c) = foldr (\e (l, c) -> (l `lub` ( joinLabels (map (\f -> f e) lfs)), c + 1)) ( bottom, 0) res




    -- lift $ foldM (\acc e -> (raiseLabelRead e) >> (return $ acc + 1)) 0 res

-- -- TODO
-- --  selectSource
-- --  selectKeys

selectList :: forall l m v backend . (PersistQuery backend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => [Filter v] -> [SelectOpt v] -> ReaderT backend (LMonadT l m) [Entity v]
selectList filts opts = do
    es <- Persist.selectList filts opts
    lift $ taintLabel $ (tableReadLabel (Proxy :: Proxy v)) `lub` ( joinLabels $ map getLabelRead es)
    return es

pSelectList :: forall l m v backend . (PersistQuery backend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, LPersistEntity l v, ProtectedEntity l v) => [Filter v] -> [SelectOpt v] -> ReaderT backend (LMonadT l m) [PEntity l v]
pSelectList filts opts = do
    let tableL = tableReadLabel (Proxy :: Proxy v)
    let lfs = concatMap filterToFields filts
    let los = concatMap selectOptToFields opts

    res' <- Persist.selectList filts opts

    let (res, l) = foldr (\e (ps, l) -> 
            let l' = lub l $ joinLabels $ map ($ e) los ++ map ($ e) lfs in
            let ps' = (toProtectedWithKey e):ps in
            (ps', l')
          ) ([], tableL) res'

    lift $ taintLabel l
    return res
    

--     l <- Persist.selectList filts opts
--     lift $ mapM toProtectedWithKey l 
-- 
-- selectKeysList :: (PersistQuery backend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => [Filter v] -> [SelectOpt v] -> ReaderT backend (LMonadT l m) [Key v]
-- selectKeysList filts opts = do
--     l <- Persist.selectList filts opts
--     lift $ mapM_ raiseLabelRead l
--     return $ map entityKey l
--     
-- -- TODO
-- --  deleteCascade
-- --  deleteCascadeWhere
-- 
-- 

-- | Helper functions.

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust m f = case m of
    Just v ->
        f v
    Nothing ->
        return ()

guardCanFlowTo :: (Label l, LMonad m) => l -> l -> LMonadT l m ()
guardCanFlowTo a b = LMonadT $
    unless (a `canFlowTo` b) $
        lift lFail

guardCanFlowToRollback :: (MonadIO m, LMonad m, Label l) => l -> l -> ReaderT SqlBackend (LMonadT l m) ()
guardCanFlowToRollback a b =
    unless (a `canFlowTo` b) $ do
        Persist.transactionUndo
        lift $ LMonadT $ lift lFail

updateHelper :: (backend ~ SqlBackend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, LPersistEntity l v) => (LMonadT l m a) -> (v -> LMonadT l m a) -> (Key v) -> [Update v] -> ReaderT backend (LMonadT l m) a
updateHelper n j key updates = do
    resM <- Persist.get key
    case resM of
        Nothing ->
            lift n
        Just oldVal -> do
            newVal <- Persist.updateGet key updates
            c <- lift getClearance
            mapM_ (\x -> guardCanFlowToRollback x c) $ getWriteLabels $ Entity key oldVal
            let newE = Entity key newVal
            mapM_ (\x -> guardCanFlowToRollback x c) $ getWriteLabels newE

            l <- lift getCurrentLabel
            mapM_ (\f -> guardCanFlowToRollback l $ f newE) $ concatMap updateToFields updates

            lift $ j newVal


--     res <- Persist.get key
--     maybe (lift n) (\oldVal -> do
--         lift $ raiseLabelWrite $ Entity key oldVal
--         newVal <- Persist.updateGet key updates
--         let newL = getLabelWrite $ Entity key newVal
--         l <- lift $ lubCurrentLabel newL
--         guard <- lift $ canSetLabel l
--         unless guard
--             -- Rollback
--             Persist.transactionUndo
--         lift $ setLabel l
--         lift $ j newVal
--       ) res
