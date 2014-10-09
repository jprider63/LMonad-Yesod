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
    , lFieldReference        :: !ReferenceDef
    , lFieldLabelAnnotations :: !(Maybe (LabelAnnotation,LabelAnnotation,LabelAnnotation))
    }
    deriving (Show, Eq, Read, Ord)

unlabelFieldDef :: LFieldDef -> FieldDef
unlabelFieldDef (LFieldDef a b c d e f g _) = 
    FieldDef a b c d e f g

unlabelEntityDef :: LEntityDef -> EntityDef
unlabelEntityDef (LEntityDef a b c d e lFs g h i j k) = 
    let fs = map unlabelFieldDef lFs in
    EntityDef a b c d e fs g h i j k

-- TODO: 
--  lShare
