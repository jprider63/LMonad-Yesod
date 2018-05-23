{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}

module Database.LPersist.TCB where

import Database.Persist (Entity(..), Key)
import LMonad.TCB

-- | ADT wrapper for protected entities. Analagous to Entity.
data PEntity l e = (ProtectedEntity l e) => PEntity (Key e) (Protected e)

-- | Typeclass for protected entities.
-- `mkLabels` automatically generates these instances.
type family Protected e = p | p -> e
class Label l => ProtectedEntity l e | e -> l where
    -- toProtected :: LMonad m => Entity e -> LMonadT l m (Protected e)
    -- TCB since we don't do any checks when labelling.
    toProtectedTCB :: Entity e -> Protected e

    -- TCB since we don't do any checks when unlabelling.
    fromProtectedTCB :: Protected e -> e

    canAllocProtected :: PEntity l e -> LMonadT l m Bool

toProtectedWithKeyTCB :: (ProtectedEntity l e) => Entity e -> PEntity l e
toProtectedWithKeyTCB r = 
    let p = toProtectedTCB r in
    PEntity (entityKey r) p

