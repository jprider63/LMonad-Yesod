{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Internal where

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Char as Char
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist.Types
import Language.Haskell.TH (Name, mkName)
import LMonad (Label(..))

data LabelAnnotation = 
    LABottom
  | LAId
  | LAConst String
  | LAField String
  | LAMeet LabelAnnotation LabelAnnotation
  | LAJoin LabelAnnotation LabelAnnotation
    deriving (Show, Eq, Read, Ord)

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
    }
    deriving (Show, Eq, Read, Ord)

type UniqueLabels = [(LabelAnnotation, LabelAnnotation)] -- , [[LabelAnnotation]])

data LFieldDef = LFieldDef
    { lFieldHaskell   :: !String -- ^ name of the field
--    , lFieldDB        :: !String
    , lFieldType      :: !FieldType
--    , lFieldSqlType   :: !SqlType
--    , lFieldAttrs     :: ![Attr]    -- ^ user annotations for a field
    , lFieldStrict    :: !Bool      -- ^ a strict field in the data type. Default: true
--    , lFieldReference :: !ReferenceDef
    , lFieldLabelAnnotations :: !(LabelAnnotation,LabelAnnotation) -- ,[LabelAnnotation])
    }
    deriving (Show, Eq, Read, Ord)

headToUpper :: String -> String
headToUpper (h:t) = (Char.toUpper h):t
headToUpper s = error $ "Invalid name `" ++ s ++ "`"

headToLower :: String -> String
headToLower (h:t) = (Char.toLower h):t
headToLower s = error $ "Invalid name `" ++ s ++ "`"

attrsToLabel attrs validator = 
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
        Just (read, write) -> (canonicalOrder read, canonicalOrder write)

    where
        defaultLabel = (LABottom,LABottom)

        canonicalOrder l@LABottom = l
        canonicalOrder l@LAId = l
        canonicalOrder l@(LAConst _) = l
        canonicalOrder l@(LAField _) = l
        canonicalOrder (LAMeet a b) = LAMeet (min a b) (max a b)
        canonicalOrder (LAJoin a b) = LAMeet (min a b) (max a b)


toLEntityDef :: EntityDef -> LEntityDef
toLEntityDef ent = 
    let fields = fmap toLFieldDef (entityFields ent) in
    let def = LEntityDef {
        lEntityHaskell = Text.unpack $ unHaskellName $ entityHaskell ent
--      , lEntityDB = Text.unpack $ unDBName $ entityDB ent
      , lEntityFields = Map.fromList fields
      , lEntityLabelAnnotations = labels
      , lEntityUniqueFieldLabelsAnnotations = lFieldsUniqueLabels fields
    }
    in
    case checkReadLabelIsBottom def of
        Nothing ->
            def
        Just err ->
            error err
    
    where
        -- JP: Do we need to sort the labels?
        labels = 
            let attrs = entityAttrs ent in
            attrsToLabel attrs $ \l@(readSide, writeSide) -> 
                -- Check that table label does not have id.
                if containsIdOrField readSide || containsIdOrField writeSide then
                    error $ "Entity `" ++ (Text.unpack $ unHaskellName $ entityHaskell ent) ++ "` cannot have label `Id` in the create annotation."
                else
                    l
            
                -- TODO: Maybe also no field??

        -- Don't allow Id or Field in table labels.
        containsIdOrField LAId = True
        containsIdOrField LABottom = False
        containsIdOrField (LAConst _) = False
        containsIdOrField (LAField _) = True
        containsIdOrField (LAMeet a b) = containsIdOrField a || containsIdOrField b
        containsIdOrField (LAJoin a b) = containsIdOrField a || containsIdOrField b

        -- createContainsId [] = False
        -- createContainsId (LAId:_) = True
        -- createContainsId (_:t) = createContainsId t

-- Returns an error message if a read label is not bottom.
checkReadLabelIsBottom :: LEntityDef -> Maybe String
checkReadLabelIsBottom def = foldr helper Nothing fields
    where
        fields = lEntityFields def

        helper _ e@(Just _) = e
        helper field Nothing = 
            let (readLabel, writeLabel) = lFieldLabelAnnotations field in
            let fName = lFieldHaskell field in
            helper' fName readLabel <|> helper' fName writeLabel

        helper' _ LAId = Nothing
        helper' _ LABottom = Nothing
        helper' _ (LAConst _) = Nothing
        helper' n (LAJoin a b) = helper' n a <|> helper' n b
        helper' n (LAMeet a b) = helper' n a <|> helper' n b
        helper' origName (LAField name) = case Map.lookup name fields of
            Nothing -> 
                error "checkReadLabelIsBottom: unreachable"
            Just field -> case lFieldLabelAnnotations field of
                (LABottom,_) -> Nothing
                _ -> Just $ "The read label of `" ++ lEntityHaskell def ++ "." ++ name ++ "` must be bottom (_) since it is part of `" ++ origName ++ "`'s label."

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


toLFieldDef :: FieldDef -> (String, LFieldDef)
toLFieldDef f = 
    let def = LFieldDef {
        lFieldHaskell = fieldName
--       , lFieldDB = Text.unpack $ unDBName $ fieldDB f
      , lFieldType = typ
      , lFieldStrict = fieldStrict f
      , lFieldLabelAnnotations = labels
    }
    in
    ( fieldName, def)

    where
        fieldName = Text.unpack $ unHaskellName $ fieldHaskell f
        -- labels = case labels' of
        --     Nothing -> ([],[])
        --     Just (read, write) -> (List.sort read, List.sort write) -- , List.sort create)
        labels = 
            let attrs = fieldAttrs f in
            attrsToLabel attrs id

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
-- L = K | _
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

        parseL = (skipSpace >> char '_' >> return LABottom) <|> parseJ

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
            
getLEntityFieldDef :: LEntityDef -> String -> LFieldDef
getLEntityFieldDef ent fName = case Map.lookup fName $ lEntityFields ent of
    Nothing ->
        error $ "getLEntityFieldDef: Could not find field `" ++ fName ++"` in entity `"++ (lEntityHaskell ent) ++"`"
    Just def ->
        def

-- readLabelIsBottom ([], _) = True
-- readLabelIsBottom _ = False

-- lEntityFieldsList :: LEntityDef -> [LFieldDef]
-- lEntityFieldsList = Map.elems . lEntityFields

lFieldsUniqueLabels :: [(String, LFieldDef)] -> UniqueLabels
lFieldsUniqueLabels fields =
    List.nub $ fmap (lFieldLabelAnnotations . snd) fields

lNameHelper' :: String -> LabelAnnotation -> String
lNameHelper' prefix la = prefix ++ toName la
    where
        toName LABottom = "Bottom"
        toName LAId = "Id"
        toName (LAConst c) = "C" ++ c
        toName (LAField f) = "F" ++ f
        toName (LAJoin a b) = "JL" ++ toName a ++ "W" ++ toName b ++ "JR"
        toName (LAMeet a b) = "ML" ++ toName a ++ "W" ++ toName b ++ "MR"

lFieldLabelName' :: String -> LabelAnnotation -> Name
lFieldLabelName' eName anns = mkName $ ( lNameHelper' ( "label" ++ eName) anns) ++ "'"

-- lFieldReadLabelName' :: String -> [LabelAnnotation] -> Name
-- lFieldReadLabelName' eName anns = mkName $ ( lNameHelper' ( "readLabel" ++ eName) anns) ++ "'"
-- 
-- lFieldWriteLabelName' :: String -> [LabelAnnotation] -> Name
-- lFieldWriteLabelName' eName anns = mkName $ ( lNameHelper' ( "writeLabel" ++ eName) anns) ++ "'"
-- 
-- lFieldCreateLabelName' :: String -> [LabelAnnotation] -> Name
-- lFieldCreateLabelName' eName anns = mkName $ ( lNameHelper' ( "createLabel" ++ eName) anns) ++ "'"

lFieldLabelVarName :: String -> LabelAnnotation -> Name
lFieldLabelVarName eName = mkName . lNameHelper' ( "_label" ++ eName)

-- lFieldReadLabelVarName :: String -> [LabelAnnotation] -> Name
-- lFieldReadLabelVarName eName = mkName . lNameHelper' ( "_readLabel" ++ eName)

lFieldLabelName :: String -> LabelAnnotation -> Name
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
joinLabels [] = bottom
joinLabels [l] = l
joinLabels (h:t) = h `lub` joinLabels t

