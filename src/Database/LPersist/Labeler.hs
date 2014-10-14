{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

module Database.LPersist.Labeler where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Text
import Database.Persist.Types
import Language.Haskell.TH
import Prelude

import LMonad

-- | `LEntity` typeclass to taint labels when reading, writing, and creating entity fields.
class Label l => LEntity l e where
    raiseLabelRead :: LMonad m => Entity e -> String -> LMonadT l m (Maybe l)
    raiseLabelWrite :: LMonad m => Entity e -> String -> LMonadT l m (Maybe l)
    raiseLabelCreate :: LMonad m => e -> String -> LMonadT l m (Maybe l)

mkLabels :: String -> [EntityDef] -> Q [Dec]
mkLabels labelS ents = 
    return []

    where
        labelType = 
            case Text.words $ Text.pack labelS of
                [] ->
                    error $ "Label `" ++ labelS ++ "` not found"
                conT:rest ->
                    if Text.length conT <= 1 || Char.isLower (Text.head conT) then
                        error $ "Invalid label type constructor `" ++ (Text.unpack conT) ++ "`"
                    else
                        let con = ConT $ mkName $ Text.unpack conT in
                        List.foldl' (\acc typ -> AppT acc (VarT (mkName (Text.unpack typ)))) con rest


-- EntityId -> String (FieldName) -> l



-- Example:
--   User
--       ident Text
--       password Text
--       email Text <Admin || Id, Id, _>
--       admin Bool
--   
--       UniqueEmail email
--       deriving Typeable

