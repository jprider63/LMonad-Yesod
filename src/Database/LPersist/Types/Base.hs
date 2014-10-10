module Database.LPersist.Types.Base where

import Data.Map (Map)
import Data.Text (Text)
import Database.Persist.Types

data LabelAnnotation = 
      LAConst Text
    | LAField Text
--    | LAForeign Text

    deriving (Eq, Read, Show, Ord)

data LEntityDef = LEntityDef
    { lEntityHaskell :: !HaskellName
    , lEntityDB      :: !DBName
    , lEntityId      :: !FieldDef
    , lEntityAttrs   :: ![Attr]
    , lEntityFields  :: ![LFieldDef]
    , lEntityUniques :: ![UniqueDef]
    , lEntityForeigns:: ![ForeignDef]
    , lEntityDerives :: ![Text]
    , lEntityExtra   :: !(Map Text [ExtraLine])
    , lEntitySum     :: !Bool
    }
    deriving (Show, Eq, Read, Ord)

data LFieldDef = LFieldDef
    { lFieldHaskell          :: !HaskellName -- ^ name of the field
    , lFieldDB               :: !DBName
    , lFieldType             :: !FieldType
    , lFieldSqlType          :: !SqlType
    , lFieldAttrs            :: ![Attr]    -- ^ user annotations for a field
    , lFieldStrict           :: !Bool      -- ^ a strict field in the data type. Default: true
    , lFieldReference        :: !LReferenceDef
    , lFieldLabelAnnotations :: !(Maybe (LabelAnnotation,LabelAnnotation,LabelAnnotation))
    }
    deriving (Show, Eq, Read, Ord)

unlabelFieldDef :: LFieldDef -> FieldDef
unlabelFieldDef (LFieldDef a b c d e f g _) = 
    FieldDef a b c d e f (unlabelReferenceDef g)

unlabelEntityDef :: LEntityDef -> EntityDef
unlabelEntityDef (LEntityDef a b c d lFs e g h i j) = 
    let fs = map unlabelFieldDef lFs in
    EntityDef a b c d fs e g h i j

-- | There are 3 kinds of references
-- 1) composite (to fields that exist in the record)
-- 2) single field
-- 3) embedded
data LReferenceDef = LNoReference
                  | LForeignRef !HaskellName !FieldType -- ^ A ForeignRef has a late binding to the EntityDef it references via HaskellName and has the Haskell type of the foreign key in the form of FieldType
                  | LEmbedRef EmbedLEntityDef
                  | LCompositeRef CompositeDef
                  deriving (Show, Eq, Read, Ord)

unlabelReferenceDef :: LReferenceDef -> ReferenceDef
unlabelReferenceDef LNoReference = NoReference
unlabelReferenceDef (LForeignRef n t) = ForeignRef n t
unlabelReferenceDef (LEmbedRef ee) = EmbedRef $ unlabelEmbedEntityDef ee
unlabelReferenceDef (LCompositeRef c) = CompositeRef c

-- | An EmbedEntityDef is the same as an EntityDef
-- But it is only used for fieldReference
-- so it only has data needed for embedding
data EmbedLEntityDef = EmbedLEntityDef
    { lEmbeddedHaskell :: !HaskellName
    , lEmbeddedFields  :: ![EmbedLFieldDef]
    } deriving (Show, Eq, Read, Ord)

unlabelEmbedEntityDef :: EmbedLEntityDef -> EmbedEntityDef
unlabelEmbedEntityDef = undefined

-- | An EmbedFieldDef is the same as a FieldDef
-- But it is only used for embeddedFields
-- so it only has data needed for embedding
data EmbedLFieldDef = EmbedLFieldDef
    { lEmFieldDB       :: !DBName
    , lEmFieldEmbed :: Maybe EmbedLEntityDef
    }
    deriving (Show, Eq, Read, Ord)

unlabelEmbedFieldDef :: EmbedLFieldDef -> EmbedFieldDef
unlabelEmbedFieldDef (EmbedLFieldDef n Nothing) = EmbedFieldDef n Nothing
unlabelEmbedFieldDef (EmbedLFieldDef n (Just e)) = EmbedFieldDef n $ Just $ unlabelEmbedEntityDef e

toEmbedLEntityDef :: LEntityDef -> EmbedLEntityDef
toEmbedLEntityDef ent = EmbedLEntityDef
  { lEmbeddedHaskell = lEntityHaskell ent
  , lEmbeddedFields = map toEmbedLFieldDef $ lEntityFields ent
  }

toEmbedLFieldDef :: LFieldDef -> EmbedLFieldDef
toEmbedLFieldDef field =
  EmbedLFieldDef { lEmFieldDB    = lFieldDB field
                 , lEmFieldEmbed = case lFieldReference field of
                     LEmbedRef em -> Just em
                     _ -> Nothing
                 }
