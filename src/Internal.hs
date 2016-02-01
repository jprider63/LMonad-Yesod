{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Internal where

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Char as Char
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist.Types

data LabelAnnotation = 
    LAId
  | LAConst String
  | LAField String
    deriving (Show, Eq, Read, Ord)

data LEntityDef = LEntityDef
    { lEntityHaskell :: !String
--     , lEntityDB      :: !String
--     , lEntityId      :: !FieldDef
--     , lEntityAttrs   :: ![Attr]
    , lEntityFields  :: ![LFieldDef]
--     , lEntityUniques :: ![UniqueDef]
--     , lEntityForeigns:: ![ForeignDef]
--     , lEntityDerives :: ![Text]
--     , lEntityExtra   :: !(Map Text [ExtraLine])
--     , lEntitySum     :: !Bool
    }
    deriving (Show, Eq, Read, Ord)

data LFieldDef = LFieldDef
    { lFieldHaskell   :: !String -- ^ name of the field
--    , lFieldDB        :: !String
    , lFieldType      :: !FieldType
--    , lFieldSqlType   :: !SqlType
--    , lFieldAttrs     :: ![Attr]    -- ^ user annotations for a field
    , lFieldStrict    :: !Bool      -- ^ a strict field in the data type. Default: true
--    , lFieldReference :: !ReferenceDef
    , lFieldLabelAnnotations :: !(Maybe ([LabelAnnotation],[LabelAnnotation],[LabelAnnotation]))
    }
    deriving (Show, Eq, Read, Ord)

headToUpper :: String -> String
headToUpper (h:t) = (Char.toUpper h):t
headToUpper s = error $ "Invalid name `" ++ s ++ "`"

headToLower :: String -> String
headToLower (h:t) = (Char.toLower h):t
headToLower s = error $ "Invalid name `" ++ s ++ "`"

toLEntityDef :: EntityDef -> LEntityDef
toLEntityDef ent = LEntityDef {
        lEntityHaskell = Text.unpack $ unHaskellName $ entityHaskell ent
--      , lEntityDB = Text.unpack $ unDBName $ entityDB ent
      , lEntityFields = map toLFieldDef (entityFields ent)
    }

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


toLFieldDef :: FieldDef -> LFieldDef
toLFieldDef f = LFieldDef {
        lFieldHaskell = Text.unpack $ unHaskellName $ fieldHaskell f
--       , lFieldDB = Text.unpack $ unDBName $ fieldDB f
      , lFieldType = typ
      , lFieldStrict = fieldStrict f
      , lFieldLabelAnnotations = labels
    }

    where
        labels = 
            let attrs = fieldAttrs f in
            List.foldl' (\acc attr -> 
                let ( prefix, affix) = Text.splitAt 9 attr in
                if acc /= Nothing || prefix /= "chevrons=" then
                    acc
                else
                    let labels@(_,_,createLabels) = parseChevrons affix in
                    -- Check that create does not have id.
                    if createContainsId createLabels then
                        error $ "Field `" ++ (Text.unpack $ unHaskellName $ fieldHaskell f) ++ "` cannot have label `Id` in the create annotation."
                    else
                        Just labels
              ) Nothing attrs
        typ = if nullable (fieldAttrs f) then
                FTApp (FTTypeCon Nothing "Maybe") $ fieldType f
            else
                fieldType f
        nullable s 
            | "Maybe" `elem` s = True
            | "nullable" `elem` s = True
            | otherwise = False
        createContainsId [] = False
        createContainsId (LAId:_) = True
        createContainsId (_:t) = createContainsId t

-- Parse chevrons
-- C = < L , L , L >
-- L = K | _
-- K = A || K | A
-- A = Id | Const name | Field name

parseChevrons :: Text -> ([LabelAnnotation],[LabelAnnotation],[LabelAnnotation])
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
            skipSpace
            _ <- char ','
            create <- parseL
            return (read,write,create)

        parseL = (skipSpace >> char '_' >> return []) <|> parseK
        
        parseK = do
            la <- parseA
            tail <- (do
                skipSpace
                _ <- char '|'
                _ <- char '|'
                parseK
              ) <|> (return [])

            return $ la:tail

        parseA = do
            skipSpace
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
            
