module Database.LPersist where

import Database.Persist
import LMonad

-- | `LEntity` typeclass to taint labels when reading, writing, and creating entity fields.
-- Internally used to raise the current label on database calls. 
-- `mkLabels` automatically generates instances of `LEntity` for your model. 
class Label l => LEntity l e where
    raiseLabelRead :: LMonad m => Entity e -> LMonadT l m ()
    raiseLabelWrite :: LMonad m => Entity e -> LMonadT l m ()
    raiseLabelCreate :: LMonad m => e -> LMonadT l m ()
