{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveGeneric #-}

module Internal where

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Char as Char
-- import Data.Hashable
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist.Types
import GHC.Generics (Generic)
import Language.Haskell.TH (Name, mkName)
import LMonad (Label(..))

data LabelAnnotation = 
    LABottom
  | LATop
  | LAId
  | LAConst String
  | LAField String
  | LAMeet LabelAnnotation LabelAnnotation
  | LAJoin LabelAnnotation LabelAnnotation
    deriving (Show, Eq, Read, Ord, Generic)

-- instance Hashable LabelAnnotation

data LEntityDef = LEntityDef
    { lEntityHaskell :: !String
--     , lEntityDB      :: !String
--     , lEntityId      :: !FieldDef
--     , lEntityAttrs   :: ![Attr]
    , lEntityFields  :: !(Map String LFieldDef)
--     , lEntityUniques :: ![UniqueDef]
--     , lEntityForeigns:: ![ForeignDef]
--     , lEntityDerives :: ![Text]
--     , lEntityExtra   :: !(Map Text [ExtraLine])
--     , lEntitySum     :: !Bool
    , lEntityLabelAnnotations :: !(LabelAnnotation,LabelAnnotation)
    , lEntityUniqueFieldLabelsAnnotations :: !UniqueLabels
    , lEntityDependencyFields :: !(Set String)
    }
    deriving (Show, Eq, Read, Ord)

type UniqueLabels = [(LabelAnnotation, LabelAnnotation)] -- Could be Set, but TH mostly uses lists

data LFieldDef = LFieldDef
    { lFieldHaskell   :: !String -- ^ name of the field
--    , lFieldDB        :: !String
    , lFieldType      :: !FieldType
--    , lFieldSqlType   :: !SqlType
--    , lFieldAttrs     :: ![Attr]    -- ^ user annotations for a field
    , lFieldStrict    :: !Bool      -- ^ a strict field in the data type. Default: true
--    , lFieldReference :: !ReferenceDef
    , lFieldLabelAnnotations :: !(LabelAnnotation,LabelAnnotation) -- ,[LabelAnnotation])
    , lFieldLabelDependencies :: ![String] -- Fields this field's label depends on. 
    }
    deriving (Show, Eq, Read, Ord)

headToUpper :: String -> String
headToUpper (h:t) = (Char.toUpper h):t
headToUpper s = error $ "Invalid name `" ++ s ++ "`"

headToLower :: String -> String
headToLower (h:t) = (Char.toLower h):t
headToLower s = error $ "Invalid name `" ++ s ++ "`"

isFieldLabeled :: LEntityDef -> LFieldDef -> Bool
isFieldLabeled ent field = lEntityLabelAnnotations ent /= lFieldLabelAnnotations field

-- Returns an attrs label.
attrsToLabel :: [Attr] -> (LabelAnnotation, LabelAnnotation) -> ((LabelAnnotation, LabelAnnotation) -> (LabelAnnotation, LabelAnnotation)) -> (LabelAnnotation, LabelAnnotation)
attrsToLabel attrs defaultLabel validator = 
    let labels = List.foldl' (\acc attr ->
          let ( prefix, affix) = Text.splitAt 9 attr in
          if acc /= Nothing || prefix /= "chevrons=" then
            acc
          else
            Just $ validator $ parseChevrons affix
         ) Nothing attrs
    in
    case labels of
        Nothing -> defaultLabel
        Just (read, write) -> (canonicalLabelAnnotationOrder read, canonicalLabelAnnotationOrder write)

canonicalLabelAnnotationOrder :: LabelAnnotation -> LabelAnnotation
canonicalLabelAnnotationOrder l@LATop = l
canonicalLabelAnnotationOrder l@LABottom = l
canonicalLabelAnnotationOrder l@LAId = l
canonicalLabelAnnotationOrder l@(LAConst _) = l
canonicalLabelAnnotationOrder l@(LAField _) = l
canonicalLabelAnnotationOrder (LAMeet a b) = LAMeet (min a b) (max a b)
canonicalLabelAnnotationOrder (LAJoin a b) = LAMeet (min a b) (max a b)


toLEntityDef :: (LabelAnnotation, LabelAnnotation) -> EntityDef -> LEntityDef
toLEntityDef defaultLabel ent = 
    LEntityDef {
        lEntityHaskell = Text.unpack $ unHaskellName $ entityHaskell ent
--      , lEntityDB = Text.unpack $ unDBName $ entityDB ent
      , lEntityFields = fields
      , lEntityLabelAnnotations = labels
      , lEntityUniqueFieldLabelsAnnotations = lFieldsUniqueLabels fields
      , lEntityDependencyFields = dependencyFields
    }
    -- case checkReadLabelIsBottom def of
    --     Nothing ->
    --         def
    --     Just err ->
    --         error err
    
    where
        fields = Map.fromList $ fmap (toLFieldDef defaultLabel) (entityFields ent)

        -- Fields that are dependencies of other field labels.
        dependencyFields = foldr (accTuple insertFields . lFieldLabelAnnotations) Set.empty fields

        accTuple f (a, b) acc = f b $ f a acc
        insertFields LABottom acc = acc
        insertFields LATop acc = acc
        insertFields LAId acc = acc
        insertFields (LAConst _) acc = acc
        insertFields (LAField s) acc = Set.insert s acc
        insertFields (LAMeet _ _) acc = acc
        insertFields (LAJoin _ _) acc = acc


        -- JP: Do we need to sort the labels?
        labels = 
            let attrs = entityAttrs ent in
            attrsToLabel attrs defaultLabel $ \l@(readSide, writeSide) -> 
                -- Check that table label does not have id or field.
                if containsIdOrField readSide || containsIdOrField writeSide then
                    error $ "Entity `" ++ (Text.unpack $ unHaskellName $ entityHaskell ent) ++ "` cannot have label `Id` or `Field` in the table label."
                else
                    l
            
                -- TODO: Maybe also no field??

        -- Don't allow Id or Field in table labels.
        containsIdOrField LAId = True
        containsIdOrField LATop = False
        containsIdOrField LABottom = False
        containsIdOrField (LAConst _) = False
        containsIdOrField (LAField _) = True
        containsIdOrField (LAMeet a b) = containsIdOrField a || containsIdOrField b
        containsIdOrField (LAJoin a b) = containsIdOrField a || containsIdOrField b

        -- createContainsId [] = False
        -- createContainsId (LAId:_) = True
        -- createContainsId (_:t) = createContainsId t

-- Returns an error message if a read label is not bottom.
-- checkReadLabelIsBottom :: LEntityDef -> Maybe String
-- checkReadLabelIsBottom def = foldr helper Nothing fields
--     where
--         fields = lEntityFields def
-- 
--         helper _ e@(Just _) = e
--         helper field Nothing = 
--             let (readLabel, writeLabel) = lFieldLabelAnnotations field in
--             let fName = lFieldHaskell field in
--             helper' fName readLabel <|> helper' fName writeLabel
-- 
--         helper' _ LAId = Nothing
--         helper' _ LABottom = Nothing
--         helper' _ (LAConst _) = Nothing
--         helper' n (LAJoin a b) = helper' n a <|> helper' n b
--         helper' n (LAMeet a b) = helper' n a <|> helper' n b
--         helper' origName (LAField name) = case Map.lookup name fields of
--             Nothing -> 
--                 error "checkReadLabelIsBottom: unreachable"
--             Just field -> case lFieldLabelAnnotations field of
--                 (LABottom,_) -> Nothing
--                 _ -> Just $ "The read label of `" ++ lEntityHaskell def ++ "." ++ name ++ "` must be bottom (_) since it is part of `" ++ origName ++ "`'s label."
-- 
-- TODO: What about integrity labels??? XXX




--     where
--         lookupField fields name = 
--             let fieldM = List.foldl' (\acc field' -> case acc of
--                     Just _ -> 
--                         acc
--                     Nothing -> 
--                         if (lFieldHaskell field') == name then
--                             Just field'
--                         else
--                             Nothing
--                   ) Nothing fields
--             in
--             maybe (error $ "Could not find field `" ++ name ++ "`") id fieldM
--         checkLabels fields = List.foldr (\field' acc -> 
--                 if labelsContainsOption fields field' then
--                     error $ "Field `" ++ (lFieldHaskell field') ++ "` cannot have a `Field` label annotation that points to an optional column."
--                 else
--                     field':acc
--             ) [] fields
--         labelsContainsOption fields field = maybe False (\(r,w,c) -> 
--             (labelsContainsOption' fields r) 
--                 || (labelsContainsOption' fields w) 
--                 || (labelsContainsOption' fields c)
--           ) (lFieldLabelAnnotations field)
--         labelsContainsOption' _ [] = False
--         labelsContainsOption' fields ((LAField name):t) = 
--             let field = lookupField fields name in
--             -- Check if it's optional. 
--             case lFieldType field of
--                 FTApp (FTTypeCon _ "Maybe") _ ->
--                     True
--                 _ ->
--                     labelsContainsOption' fields t
--         labelsContainsOption' fields (_:t) = labelsContainsOption' fields t


toLFieldDef :: (LabelAnnotation, LabelAnnotation) -> FieldDef -> (String, LFieldDef)
toLFieldDef defaultLabel f = 
    let def = LFieldDef {
        lFieldHaskell = fieldName
--       , lFieldDB = Text.unpack $ unDBName $ fieldDB f
      , lFieldType = typ
      , lFieldStrict = fieldStrict f
      , lFieldLabelAnnotations = labels
      , lFieldLabelDependencies = dependencies
    }
    in
    ( fieldName, def)

    where
        dependenciesHelper LAId acc = "id":acc
        dependenciesHelper (LAField f) acc = f:acc
        dependenciesHelper (LAConst _) acc = acc
        dependenciesHelper LABottom acc = acc
        dependenciesHelper LATop acc = acc
        dependenciesHelper (LAJoin a b) acc = dependenciesHelper a $ dependenciesHelper b acc
        dependenciesHelper (LAMeet a b) acc = dependenciesHelper a $ dependenciesHelper b acc


        dependencies = List.nub $ dependenciesHelper (snd labels) $ dependenciesHelper (fst labels) []

        fieldName = Text.unpack $ unHaskellName $ fieldHaskell f
        -- labels = case labels' of
        --     Nothing -> ([],[])
        --     Just (read, write) -> (List.sort read, List.sort write) -- , List.sort create)
        labels = 
            let attrs = fieldAttrs f in
            attrsToLabel attrs defaultLabel id

        typ = if nullable (fieldAttrs f) then
                FTApp (FTTypeCon Nothing "Maybe") $ fieldType f
            else
                fieldType f
        nullable s 
            | "Maybe" `elem` s = True
            | "nullable" `elem` s = True
            | otherwise = False

-- Parse chevrons
-- C = < L , L >
-- L = K | _ | ^
-- K = A || K | A
-- A = Id | Const name | Field name

parseChevrons :: Text -> (LabelAnnotation,LabelAnnotation)
parseChevrons s = case parseOnly parseC s of
    Left err ->
        error $ "Could not parse labels in chevrons: " ++ err
    Right res ->
        res

    where
        parseC = do
            read <- parseL
            skipSpace
            _ <- char ','
            write <- parseL
            return (read,write)

        parseL = 
                (skipSpace >> char '_' >> return LABottom) 
            <|> (skipSpace >> char '^' >> return LATop) 
            <|> parseJ

        parseJ = do
            lm <- parseM
            tail <- (do
                skipSpace
                _ <- char '\\'
                _ <- char '/'
                parseJ
              ) <|> (return lm)

            return $ LAJoin lm tail

        parseM = do
            la <- parseA
            tail <- (do
                skipSpace
                _ <- char '/'
                _ <- char '\\'
                parseM
              ) <|> (return la)

            return $ LAMeet la tail

        parseA = do
            skipSpace
            parseParens <|> parseTerm

        parseParens = do
            _ <- char '('
            j <- parseJ
            skipSpace
            _ <- char ')'
            return j

        parseTerm = do
            constr <- takeAlphaNum
            case constr of
                "Id" -> 
                    return LAId
                "Const" -> do
                    skipSpace
                    name <- takeAlphaNum
                    return $ LAConst $ Text.unpack name
                "Field" -> do
                    skipSpace
                    name <- takeAlphaNum
                    return $ LAField $ Text.unpack name
                _ -> 
                    fail $ "Unknown keyword `" ++ (Text.unpack constr) ++ "` in parseChevrons. Use `Id`, `Const`, `Field`, or `_`"
        
        takeAlphaNum = takeWhile1 Char.isAlphaNum
            
getLEntityFieldOrIdDef :: LEntityDef -> String -> LFieldDef
getLEntityFieldOrIdDef ent "id" = 
    let tableS = lEntityHaskell ent in
    let tLabel = lEntityLabelAnnotations ent in
    let typ = FTTypeCon Nothing (Text.pack $ tableS ++ "Id") in
    LFieldDef "id" typ True tLabel []
getLEntityFieldOrIdDef ent fName = getLEntityFieldDef ent fName

getLEntityFieldDef :: LEntityDef -> String -> LFieldDef
getLEntityFieldDef ent fName = case Map.lookup fName $ lEntityFields ent of
    Nothing ->
        error $ "getLEntityFieldDef: Could not find field `" ++ fName ++"` in entity `"++ (lEntityHaskell ent) ++"`"
    Just def ->
        def

-- labelIsBottom (LABottom, LABottom) = True
-- labelIsBottom _ = False

-- lEntityFieldsList :: LEntityDef -> [LFieldDef]
-- lEntityFieldsList = Map.elems . lEntityFields

lFieldsUniqueLabels :: Map String LFieldDef -> UniqueLabels
lFieldsUniqueLabels fields =
    Set.toList $ foldMap (Set.singleton . lFieldLabelAnnotations) fields

lNameHelper' :: String -> (LabelAnnotation, LabelAnnotation) -> String
lNameHelper' prefix (la, lb) = prefix ++ toName la ++ "NM" ++ toName lb
    where
        toName LATop = "Top"
        toName LABottom = "Bottom"
        toName LAId = "Id"
        toName (LAConst c) = "C" ++ c
        toName (LAField f) = "F" ++ f
        toName (LAJoin a b) = "JL" ++ toName a ++ "W" ++ toName b ++ "JR"
        toName (LAMeet a b) = "ML" ++ toName a ++ "W" ++ toName b ++ "MR"

lFieldLabelName' :: String -> (LabelAnnotation, LabelAnnotation) -> Name
lFieldLabelName' eName anns = mkName $ ( lNameHelper' ( "label" ++ eName) anns) ++ "'"

-- lFieldReadLabelName' :: String -> [LabelAnnotation] -> Name
-- lFieldReadLabelName' eName anns = mkName $ ( lNameHelper' ( "readLabel" ++ eName) anns) ++ "'"
-- 
-- lFieldWriteLabelName' :: String -> [LabelAnnotation] -> Name
-- lFieldWriteLabelName' eName anns = mkName $ ( lNameHelper' ( "writeLabel" ++ eName) anns) ++ "'"
-- 
-- lFieldCreateLabelName' :: String -> [LabelAnnotation] -> Name
-- lFieldCreateLabelName' eName anns = mkName $ ( lNameHelper' ( "createLabel" ++ eName) anns) ++ "'"

lFieldLabelVarName :: String -> (LabelAnnotation, LabelAnnotation) -> Name
lFieldLabelVarName eName = mkName . lNameHelper' ( "_label" ++ eName)

-- lFieldReadLabelVarName :: String -> [LabelAnnotation] -> Name
-- lFieldReadLabelVarName eName = mkName . lNameHelper' ( "_readLabel" ++ eName)

lFieldLabelName :: String -> (LabelAnnotation, LabelAnnotation) -> Name
lFieldLabelName eName = mkName . lNameHelper' ( "label" ++ eName)

-- lFieldReadLabelName :: String -> [LabelAnnotation] -> Name
-- lFieldReadLabelName eName = mkName . lNameHelper' ( "readLabel" ++ eName)
-- 
-- lFieldWriteLabelName :: String -> [LabelAnnotation] -> Name
-- lFieldWriteLabelName eName = mkName . lNameHelper' ( "writeLabel" ++ eName)
-- 
-- lFieldCreateLabelName :: String -> [LabelAnnotation] -> Name
-- lFieldCreateLabelName eName = mkName . lNameHelper' ( "createLabel" ++ eName)

joinLabels :: Label l => [l] -> l
joinLabels [] = bottom -- JP: Default label?
joinLabels (h:t) = List.foldr (\l acc -> l `lub` acc) h t
-- joinLabels [] = bottom
-- joinLabels [l] = l
-- joinLabels (h:t) = h `lub` joinLabels t

lFieldLabelArguments :: (LabelAnnotation, LabelAnnotation) -> [LabelAnnotation]
lFieldLabelArguments (la, lb) = 
    let leaves = labelAnnotationLeaves la ++ labelAnnotationLeaves lb in
    List.sort $ List.nub leaves

    where
        labelAnnotationLeaves :: LabelAnnotation -> [LabelAnnotation]
        labelAnnotationLeaves LATop = [LATop]
        labelAnnotationLeaves LABottom = [LABottom]
        labelAnnotationLeaves LAId = [LAId]
        labelAnnotationLeaves l@(LAConst _) = [l]
        labelAnnotationLeaves l@(LAField _) = [l]
        labelAnnotationLeaves (LAMeet l1 l2) = labelAnnotationLeaves l1 ++ labelAnnotationLeaves l2
        labelAnnotationLeaves (LAJoin l1 l2) = labelAnnotationLeaves l1 ++ labelAnnotationLeaves l2

