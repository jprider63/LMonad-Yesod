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

{-# LANGUAGE TypeFamilies, ScopedTypeVariables, FlexibleContexts #-}

module Database.LPersist (
      LEntity(..)
    , getEntityLabel
    , YesodLPersist (..)
    , lDefaultRunDB
    , ProtectedEntity
    , Protected
    , PEntity(..)
    , get
    , pGet
    , insert
    , insert_
    , insertMany
    , insertKey
    , pInsert
    , pInsert_
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
    , deleteBy
--    , insertUnique
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
import qualified Control.Monad.Trans.State as ST
import Data.Proxy
import Database.Persist (Entity(..),EntityField(..),PersistStore,PersistEntity,PersistEntityBackend, Key, Update(..), Unique, PersistUnique, SelectOpt, Filter(..), PersistQuery, SelectOpt(..), BaseBackend, PersistQueryRead, ToBackendKey, PersistStoreWrite, PersistStoreRead)
import qualified Database.Persist as Persist
import Database.Persist.Sql (SqlBackend, PersistConfig, PersistConfigPool, PersistConfigBackend)
import qualified Database.Persist.Sql as Persist
import qualified Data.Text as Text
import LMonad.TCB
import Yesod.Core
import Yesod.Persist (YesodPersist(..))

import Database.LPersist.TCB
import Internal

-- | `LEntity` typeclass to taint labels when reading, writing, and creating entity fields.
-- Internally used to raise the current label on database calls. 
-- `mkLabels` automatically generates instances of `LEntity` for your model. 
class (Label l) => LEntity l e where
    getFieldLabels :: Entity e -> [l]

    -- getReadLabels :: Entity e -> [l]
    -- getWriteLabels :: Entity e -> [l]
    -- getCreateLabels :: e -> [l]

    tableLabel :: Proxy e -> l

    -- tableReadLabel :: Proxy e -> l
    -- tableInsertLabel :: e -> l

-- class Label l => LEntityField l e t where
--     fieldReadLabel :: EntityField e t -> Entity e -> l

class (Label l, PersistEntity e) => LPersistEntity l e | e -> l where
    lPersistFieldLabel :: EntityField e t -> Entity e -> l
    lPersistReadLabelDependencies :: EntityField e t -> [EntityField e v]
    lPersistUniqueFields :: Unique e -> [EntityField e v]

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

uniqueToFields :: forall l e . (LPersistEntity l e) => Unique e -> [Entity e -> l]
uniqueToFields uniq = map lPersistFieldLabel $ lPersistUniqueFields uniq

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

-- | Gets the join of all the fields of an `Entity`.
getEntityLabel :: LEntity l e => Entity e -> l
getEntityLabel = joinLabels . getFieldLabels

-- raiseLabelRead :: (Label l, LMonad m, LEntity l e) => Entity e -> LMonadT l m ()
-- raiseLabelRead e = taintLabel $ getLabelRead e

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

get :: forall l m v backend . (LMonad m, Label l, LEntity l v, MonadIO m, PersistEntityBackend v ~ BaseBackend backend, PersistStoreRead backend, PersistEntity v) => Key v -> ReaderT backend (LMonadT l m) (Maybe v)
get key = do
    let tLabel = tableLabel (Proxy :: Proxy v)
    -- Key label equals table label, so we don't need to check it.
    res <- Persist.get key
    lift $ taintLabel $ maybe 
        tLabel
        (\v -> tLabel `lub` (getEntityLabel $ Entity key v)) res
    return res

pGet :: forall l m v backend . (ProtectedEntity l v, LMonad m, Label l, LEntity l v, MonadIO m, PersistEntityBackend v ~ BaseBackend backend, PersistStoreRead backend, PersistEntity v) => Key v -> ReaderT backend (LMonadT l m) (Maybe (Protected v))
pGet key = do
    lift $ taintLabel $ tableLabel (Proxy :: Proxy v)
    res <- Persist.get key
    return $ fmap (toProtectedTCB . Entity key) res

-- Helper function for inserts.
insertHelper :: forall l m v backend . (LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v, backend ~ SqlBackend) => v -> ReaderT backend (LMonadT l m) (Key v)
insertHelper val = do
    let tLabel = tableLabel (Proxy :: Proxy v)
    lift $ guardAlloc tLabel

    k <- Persist.insert val
    let e = Entity k val
    mapM_ guardAllocRollback $ getFieldLabels e
    return k

insert :: forall l m v . (LMonad m, Label l, LEntity l v, MonadIO m, PersistEntityBackend v ~ SqlBackend, PersistEntity v) => v -> ReaderT SqlBackend (LMonadT l m) (Key v)
insert val = do
    -- Insert value.
    k <- insertHelper val

    -- Raise to table label since we're reading key.
    -- JP: Should we rollback?
    lift $ taintLabel $ tableLabel (Proxy :: Proxy v)

    return k

-- Helper function for protected inserts.
pInsertHelper :: forall m l e . (MonadIO m, LMonad m, Label l, LEntity l e, ProtectedEntity l e, PersistEntity e, PersistEntityBackend e ~ SqlBackend) => Protected e -> ReaderT SqlBackend (LMonadT l m) (Key e)
pInsertHelper p = do
    let tLabel = tableLabel (Proxy :: Proxy e)
    lift $ guardAlloc tLabel

    k <- Persist.insert $ fromProtectedTCB p
    let pe = PEntity k p

    -- Rollback if cannot insert.
    succ <- lift $ canAllocProtected pe
    unless succ $ 
        rollback

    return k

pInsert :: forall m l e . (MonadIO m, LMonad m, Label l, LEntity l e, ProtectedEntity l e, PersistEntity e, PersistEntityBackend e ~ SqlBackend) => Protected e -> ReaderT SqlBackend (LMonadT l m) (Key e)
pInsert val = do
    -- Insert value.
    k <- pInsertHelper val

    -- Raise to table label since we're reading key.
    -- JP: Should we rollback?
    lift $ taintLabel $ tableLabel (Proxy :: Proxy e)

    return k

insert_ :: (LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v, backend ~ SqlBackend) => v -> ReaderT backend (LMonadT l m) ()
insert_ val = do
    _ <- insertHelper val
    return ()

pInsert_ :: forall m l e . (MonadIO m, LMonad m, Label l, LEntity l e, ProtectedEntity l e, PersistEntity e, PersistEntityBackend e ~ SqlBackend) => Protected e -> ReaderT SqlBackend (LMonadT l m) ()
pInsert_ val = do
    _ <- pInsertHelper val
    return ()

insertMany :: (LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v, backend ~ SqlBackend) => [v] -> ReaderT backend (LMonadT l m) [Key v]
insertMany vals = mapM insert vals
-- JP: There are some redundant checks we can get rid of.

------ Done ------

insertKey :: forall l m v backend . (LMonad m, Label l, LEntity l v, MonadIO m, PersistEntityBackend v ~ BaseBackend backend, PersistStoreWrite backend, PersistEntity v) => (Key v) -> v -> ReaderT backend (LMonadT l m) ()
insertKey key val = do
    let tLabel = tableLabel (Proxy :: Proxy v)
    c <- lift getClearance
    l <- lift getCurrentLabel
    lift $ mapM_ (\j -> guardCanFlowTo l j >> guardCanFlowTo j c) $ tLabel:(getFieldLabels $ Entity key val)
    Persist.insertKey key val

-- repsert :: (LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v) => (Key v) -> v -> ReaderT backend (LMonadT l m) ()
-- repsert key val = do
--     lift $ raiseLabelCreate val
--     res <- Persist.get key
--     whenJust res $ lift . raiseLabelWrite . (Entity key)
--     Persist.repsert key val

replace :: (LMonad m, Label l, LEntity l v, MonadIO m, PersistEntityBackend v ~ BaseBackend backend, PersistStoreWrite backend, PersistEntity v) => (Key v) -> v -> ReaderT backend (LMonadT l m) ()
replace key val = do
    let e = Entity key val
    l <- lift getCurrentLabel
    c <- lift getClearance
    lift $ mapM_ (\j -> guardCanFlowTo l j >> guardCanFlowTo j c) $ getFieldLabels e
    oldM <- Persist.get key
    whenJust oldM $ \old -> do
        lift $ mapM_ (\x -> guardCanFlowTo l x >> guardCanFlowTo x c) $ getFieldLabels $ Entity key old
        Persist.replace key val

delete :: forall l m v backend . (LMonad m, Label l, LEntity l v, MonadIO m, PersistEntityBackend v ~ BaseBackend backend, PersistStoreWrite backend, PersistEntity v) => (Key v) -> ReaderT backend (LMonadT l m) ()
delete key = do
    let tLabel = tableLabel (Proxy :: Proxy v)
    l <- lift getCurrentLabel
    c <- lift getClearance
    lift $ guardCanFlowTo l tLabel >> guardCanFlowTo tLabel c
    res <- Persist.get key
    whenJust res $ \val -> do
        -- lift $ mapM_ (\x -> guardCanFlowTo x c) $ getFieldLabels $ Entity key val
        lift $ mapM_ (\x -> guardCanFlowTo l x >> guardCanFlowTo x c) $ getFieldLabels $ Entity key val
        Persist.delete key

-- | This function only works for SqlBackends since we need to be able to rollback transactions.
update :: (backend ~ SqlBackend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, LPersistEntity l v) => (Key v) -> [Update v] -> ReaderT backend (LMonadT l m) ()
update = updateHelper (return ()) $ const $ return ()


updateGet :: forall backend l m v . (backend ~ SqlBackend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, LPersistEntity l v) => (Key v) -> [Update v] -> ReaderT backend (LMonadT l m) v
updateGet key = updateHelper err f key
    where
        f e = do
            taintLabel $ tLabel `lub` (getEntityLabel $ Entity key e)
            return e

        tLabel = tableLabel (Proxy :: Proxy v)
        err = do
            taintLabel tLabel
            liftIO $ throwIO $ Persist.KeyNotFound $ Prelude.show key

pUpdateGet :: forall l m v backend . (backend ~ SqlBackend, ProtectedEntity l v, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, LPersistEntity l v) => (Key v) -> [Update v] -> ReaderT backend (LMonadT l m) (Protected v)
pUpdateGet key updates = do
    lift $ taintLabel $ tableLabel (Proxy :: Proxy v)
    updateHelper err (return . toProtectedTCB . (Entity key)) key updates

    where
        err = liftIO $ throwIO $ Persist.KeyNotFound $ Prelude.show key

getJust :: (LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, PersistEntity v, ToBackendKey SqlBackend v) => (Key v) -> ReaderT backend (LMonadT l m) v
getJust key = get key >>= maybe err return
    where
        err = liftIO $ throwIO $ Persist.PersistForeignConstraintUnmet $ Text.pack $ show $ Persist.fromSqlKey key

pGetJust :: (LMonad m, Label l, LEntity l v, MonadIO m, ProtectedEntity l v, PersistEntityBackend v ~ backend, ToBackendKey SqlBackend v) => (Key v) -> ReaderT backend (LMonadT l m) (Protected v)
pGetJust key = pGet key >>= maybe err return
    where
        err = liftIO $ throwIO $ Persist.PersistForeignConstraintUnmet $ Text.pack $ show $ Persist.fromSqlKey key

-- -- TODO
-- --
-- -- belongsTo
-- -- belongsToJust

getBy :: forall l m v backend . (PersistUnique backend, LMonad m, Label l, LEntity l v, MonadIO m, PersistEntityBackend v ~ BaseBackend backend, PersistEntity v) => Unique v -> ReaderT backend (LMonadT l m) (Maybe (Entity v))
getBy uniq = do
    let tLabel = tableLabel (Proxy :: Proxy v)
    res <- Persist.getBy uniq
    lift $ taintLabel $ maybe tLabel (\e -> tLabel `lub` getEntityLabel e) res
    return res

pGetBy :: forall l m v backend . (ProtectedEntity l v, PersistUnique backend, LMonad m, Label l, LEntity l v, MonadIO m, LPersistEntity l v, PersistEntityBackend v ~ BaseBackend backend) => Unique v -> ReaderT backend (LMonadT l m) (Maybe (PEntity l v))
pGetBy uniq = do
    let tLabel = tableLabel (Proxy :: Proxy v)
    resM <- Persist.getBy uniq
    lift $ case resM of
        Nothing -> do
            taintLabel tLabel
            return Nothing
        Just e -> do
            taintLabel $ lub tLabel $ joinLabels $ map ($ e) $ uniqueToFields uniq

            return $ Just $ toProtectedWithKeyTCB e

deleteBy :: forall l v m backend . (ProtectedEntity l v, PersistUnique backend, LMonad m, Label l, LEntity l v, MonadIO m, PersistEntityBackend v ~ BaseBackend backend, PersistEntity v) => Unique v -> ReaderT backend (LMonadT l m) ()
deleteBy uniq = do
    let tLabel = tableLabel (Proxy :: Proxy v)
    l <- lift getCurrentLabel
    c <- lift getClearance
    lift $ guardCanFlowTo l tLabel >> guardCanFlowTo tLabel c
    resM <- Persist.getBy uniq
    whenJust resM $ \val -> do
        lift $ mapM_ (\x -> guardCanFlowTo l x >> guardCanFlowTo x c) $ getFieldLabels val
        Persist.delete $ entityKey val
            
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

updateWhere :: forall backend m v l . (backend ~ SqlBackend, PersistQuery backend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, LPersistEntity l v) => [Filter v] -> [Update v] -> ReaderT backend (LMonadT l m) ()
updateWhere filts updates = do
    res <- Persist.selectList filts []
    c <- lift getClearance
    l <- lift getCurrentLabel
    mapM_ (\oldE@(Entity key _) -> do
        lift $ mapM_ (\x -> guardCanFlowTo l x >> guardCanFlowTo x c) $ getFieldLabels oldE

        newVal <- Persist.updateGet key updates  
        let newE = Entity key newVal
        mapM_ (\x -> guardCanFlowToRollback l x >> guardCanFlowToRollback x c) $ getFieldLabels newE

        mapM_ (\f -> do
            let newL = f newE
            guardCanFlowToRollback (f oldE) newL
            guardCanFlowToRollback newL c
          ) $ concatMap updateToFields updates -- JP: Eliminate duplicates as optimization?
      ) res
    -- TODO: Read checks on the filters?



--     res <- Persist.selectList filts []
--     -- `updateGet` should rollback transaction if any checks fail
--     mapM_ (\e -> 
--         let k = entityKey e in 
--         (lift $ raiseLabelWrite e) >> 
--             (updateGet (entityKey e) upts) >>=
--                 (lift . raiseLabelWrite . (Entity k))
--       ) res

deleteWhere :: forall l m v backend . (PersistQuery backend, LMonad m, Label l, LEntity l v, MonadIO m, PersistEntityBackend v ~ BaseBackend backend, PersistEntity v) => [Filter v] -> ReaderT backend (LMonadT l m) ()
deleteWhere filts = do
    let tLabel = tableLabel (Proxy :: Proxy v)
    l <- lift getCurrentLabel
    c <- lift getClearance
    lift $ guardCanFlowTo l tLabel >> guardCanFlowTo tLabel c
    res <- Persist.selectList filts []
    lift $ mapM_ (mapM_ (\x -> guardCanFlowTo l x >> guardCanFlowTo x c) . getFieldLabels) res
    Persist.deleteWhere filts

--     res <- Persist.selectList filts []
--     lift $ mapM_ raiseLabelWrite res
--     Persist.deleteWhere filts

selectFirst :: forall l m v backend . (LMonad m, Label l, LEntity l v, MonadIO m, PersistEntityBackend v ~ BaseBackend backend, PersistEntity v, PersistQueryRead backend) => [Filter v] -> [SelectOpt v] -> ReaderT backend (LMonadT l m) (Maybe (Entity v))
selectFirst filts opts = do
    resM <- Persist.selectFirst filts opts
    let tableL = tableLabel (Proxy :: Proxy v)
    lift $ case resM of
        Nothing -> do
            taintLabel tableL
            return resM
        Just res -> do
            taintLabel $ tableL `lub` (getEntityLabel res)
            return resM

pSelectFirst :: forall backend l m v . (LMonad m, Label l, LEntity l v, MonadIO m, LPersistEntity l v, ProtectedEntity l v, PersistEntityBackend v ~ BaseBackend backend, PersistQueryRead backend) => [Filter v] -> [SelectOpt v] -> ReaderT backend (LMonadT l m) (Maybe (PEntity l v))
pSelectFirst filts opts = do
    let tableL = tableLabel (Proxy :: Proxy v)

    resM <- Persist.selectFirst filts opts
    lift $ case resM of
        Nothing -> do
            taintLabel tableL

            return Nothing
        Just e -> do
            let lfs = concatMap filterToFields filts
            let los = concatMap selectOptToFields opts
            -- JP: Eliminate duplicates as optimization?
            taintLabel $ lub tableL $ joinLabels $ map ($ e) los ++ map ($ e) lfs

            return $ Just $ toProtectedWithKeyTCB e

count :: forall l m v backend . (LMonad m, Label l, LEntity l v, MonadIO m, LPersistEntity l v, PersistEntityBackend v ~ BaseBackend backend, PersistQueryRead backend) => [Filter v] -> ReaderT backend (LMonadT l m) Int
count filts = do
    res <- Persist.selectList filts []

    let tableL = tableLabel (Proxy :: Proxy v)
    let lfs = concatMap filterToFields filts
    lift $ taintLabel $ lub tableL $ joinLabels $ concatMap (\e -> map ($ e) lfs) res

    return $ length res

    -- let (l, c) = foldr (\e (l, c) -> (l `lub` ( joinLabels (map (\f -> f e) lfs)), c + 1)) ( bottom, 0) res




    -- lift $ foldM (\acc e -> (raiseLabelRead e) >> (return $ acc + 1)) 0 res

-- -- TODO
-- --  selectSource
-- --  selectKeys

selectList :: forall l m v backend . (LMonad m, Label l, LEntity l v, MonadIO m, PersistEntityBackend v ~ BaseBackend backend, PersistQueryRead backend, PersistEntity v) => [Filter v] -> [SelectOpt v] -> ReaderT backend (LMonadT l m) [Entity v]
selectList filts opts = do
    es <- Persist.selectList filts opts
    lift $ taintLabel $ (tableLabel (Proxy :: Proxy v)) `lub` ( joinLabels $ map getEntityLabel es)
    return es

pSelectList :: forall l m v backend . (LMonad m, Label l, LEntity l v, MonadIO m, LPersistEntity l v, ProtectedEntity l v, PersistEntityBackend v ~ BaseBackend backend, PersistQueryRead backend) => [Filter v] -> [SelectOpt v] -> ReaderT backend (LMonadT l m) [PEntity l v]
pSelectList filts opts = do
    let tableL = tableLabel (Proxy :: Proxy v)
    let lfs = concatMap filterToFields filts
    let los = concatMap selectOptToFields opts
    -- JP: Eliminate duplicates as optimization?

    res' <- Persist.selectList filts opts

    let (res, l) = foldr (\e (ps, l) -> 
            let l' = lub l $ joinLabels $ map ($ e) los ++ map ($ e) lfs in
            let ps' = (toProtectedWithKeyTCB e):ps in
            (ps', l')
          ) ([], tableL) res'

    lift $ taintLabel l
    return res
    

selectKeysList :: forall l m v backend . (LMonad m, Label l, LEntity l v, MonadIO m, LPersistEntity l v, PersistEntityBackend v ~ BaseBackend backend, PersistQueryRead backend) => [Filter v] -> [SelectOpt v] -> ReaderT backend (LMonadT l m) [Key v]
selectKeysList filts opts = do
    res' <- Persist.selectList filts opts
    
    let tableL = tableLabel (Proxy :: Proxy v)
    let lfs = concatMap filterToFields filts
    let los = concatMap selectOptToFields opts
    -- JP: Eliminate duplicates as optimization?

    let (res, l) = foldr (\e (ps, l) -> 
            let l' = lub l $ joinLabels $ map ($ e) los ++ map ($ e) lfs in
            let ps' = (entityKey e):ps in
            (ps', l')
          ) ([], tableL) res'

    lift $ taintLabel l
    return res

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

guardAlloc :: (Label l, LMonad m) => l -> LMonadT l m ()
guardAlloc l = LMonadT $ do
    (LState label clearance) <- ST.get
    unless (canFlowTo label l && canFlowTo l clearance) $
        lift lFail

-- Rollback and LMonad fail.
rollback :: (Label l, LMonad m, MonadIO m) => ReaderT SqlBackend (LMonadT l m) ()
rollback = do
    Persist.transactionUndo
    lift $ LMonadT $ lift lFail

guardAllocRollback :: (Label l, LMonad m, MonadIO m) => l -> ReaderT SqlBackend (LMonadT l m) ()
guardAllocRollback l = do
    (LState label clearance) <- lift $ LMonadT ST.get
    unless (canFlowTo label l && canFlowTo l clearance) $
        rollback

guardCanFlowToRollback :: (MonadIO m, LMonad m, Label l) => l -> l -> ReaderT SqlBackend (LMonadT l m) ()
guardCanFlowToRollback a b =
    unless (a `canFlowTo` b) $ do
        rollback

updateHelper :: (backend ~ SqlBackend, LMonad m, Label l, LEntity l v, MonadIO m, PersistStore backend, backend ~ PersistEntityBackend v, LPersistEntity l v) => (LMonadT l m a) -> (v -> LMonadT l m a) -> (Key v) -> [Update v] -> ReaderT backend (LMonadT l m) a
updateHelper n j key updates = do
    resM <- Persist.get key
    case resM of
        Nothing ->
            lift n
        Just oldVal -> do
            l <- lift getCurrentLabel
            c <- lift getClearance
            let oldE = Entity key oldVal
            lift $ mapM_ (\x -> guardCanFlowTo l x >> guardCanFlowTo x c) $ getFieldLabels oldE
            newVal <- Persist.updateGet key updates
            let newE = Entity key newVal
            mapM_ (\x -> guardCanFlowToRollback l x >> guardCanFlowToRollback x c) $ getFieldLabels newE

            mapM_ (\f -> do
                let newL = f newE
                guardCanFlowToRollback (f oldE) newL
                guardCanFlowToRollback newL c
              ) $ concatMap updateToFields updates -- JP: Eliminate duplicates as optimization?

            lift $ j newVal

