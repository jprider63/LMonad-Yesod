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
    { entityHaskell :: !HaskellName
    , entityDB      :: !DBName
    , entityId      :: !FieldDef
    , entityAttrs   :: ![Attr]
    , entityFields  :: ![LFieldDef]
    , entityUniques :: ![UniqueDef]
    , entityForeigns:: ![ForeignDef]
    , entityDerives :: ![Text]
    , entityExtra   :: !(Map Text [ExtraLine])
    , entitySum     :: !Bool
    }
    deriving (Show, Eq, Read, Ord)

data LFieldDef = LFieldDef
    { fieldHaskell          :: !HaskellName -- ^ name of the field
    , fieldDB               :: !DBName
    , fieldType             :: !FieldType
    , fieldSqlType          :: !SqlType
    , fieldAttrs            :: ![Attr]    -- ^ user annotations for a field
    , fieldStrict           :: !Bool      -- ^ a strict field in the data type. Default: true
    , fieldReference        :: !ReferenceDef
    , fieldLabelAnnotations :: !(Maybe (LabelAnnotation,LabelAnnotation,LabelAnnotation))
    }
    deriving (Show, Eq, Read, Ord)

-- TODO: 
--  unlabelEntityDef :: LEntityDef -> EntityDef XXX
--  lShare
