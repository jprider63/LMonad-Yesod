{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, OverloadedStrings #-}

module Database.LPersist.Labeler (mkLabels) where

import Control.Applicative
import qualified Data.Char as Char
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Attoparsec.Text
import Database.Persist.Types
import Language.Haskell.TH
import Prelude

import LMonad

-- Functions that use TH to generate labeling code. 

mkLabels :: String -> [EntityDef] -> Q [Dec]
mkLabels labelS ents = 
    let entsL = map toLEntityDef ents in
    do
    protected <- mapM (mkProtected labelType) entsL
    return protected

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

-- | Create protected ADTs for the models in Persist's DSL. 
-- Ex: ProtectedUser is created for protected version of User.
--
-- data ProtectedUser = ProtectedUser {
--         pUserIdent :: Text
--       , pUserPassword :: Text
--       , pUserEmail :: Labeled (DCLabel Principal) Text
--       , pUserAdmin :: Bool
--     }
mkProtected :: Type -> LEntityDef -> Q Dec
mkProtected labelType ent =
    let pFields = map mkProtectedField (lEntityFields ent) in
    return $ DataD [] pName [] [RecC pName pFields] []

    where
        pName = mkName $ "Protected" ++ (Text.unpack $ unHaskellName $ lEntityHaskell ent)
        mkProtectedField field = 
            let name = mkName "TODO" in
            let strict = if lFieldStrict field then IsStrict else NotStrict in
            let rawType = fieldTypeToType $ lFieldType field in
            let typ = case lFieldLabelAnnotations field of
                  Nothing ->
                    rawType
                  Just _ ->
                    AppT (AppT (ConT (mkName "Labeled")) labelType) rawType
            in
            (name, strict, typ)

-- | `LEntity` typeclass to taint labels when reading, writing, and creating entity fields.
class Label l => LEntity l e where
    raiseLabelRead :: LMonad m => Entity e -> String -> LMonadT l m (Maybe l)
    raiseLabelWrite :: LMonad m => Entity e -> String -> LMonadT l m (Maybe l)
    raiseLabelCreate :: LMonad m => e -> String -> LMonadT l m (Maybe l)

data LEntityDef = LEntityDef
    { lEntityHaskell :: !HaskellName
    , lEntityDB      :: !DBName
--     , lEntityId      :: !FieldDef
--     , lEntityAttrs   :: ![Attr]
    , lEntityFields  :: ![LFieldDef]
--     , lEntityUniques :: ![UniqueDef]
--     , lEntityForeigns:: ![ForeignDef]
--     , lEntityDerives :: ![Text]
--     , lEntityExtra   :: !(Map Text [ExtraLine])
--     , lEntitySum     :: !Bool
    }

data LFieldDef = LFieldDef
    { lFieldHaskell   :: !HaskellName -- ^ name of the field
    , lFieldDB        :: !DBName
    , lFieldType      :: !FieldType
--    , lFieldSqlType   :: !SqlType
--    , lFieldAttrs     :: ![Attr]    -- ^ user annotations for a field
    , lFieldStrict    :: !Bool      -- ^ a strict field in the data type. Default: true
--    , lFieldReference :: !ReferenceDef
    , lFieldLabelAnnotations :: !(Maybe ([LabelAnnotation],[LabelAnnotation],[LabelAnnotation]))
    }
    deriving (Show, Eq, Read, Ord)

toLEntityDef :: EntityDef -> LEntityDef
toLEntityDef ent = LEntityDef {
        lEntityHaskell = entityHaskell ent
      , lEntityDB = entityDB ent
      , lEntityFields = map toLFieldDef (entityFields ent)
    }

toLFieldDef :: FieldDef -> LFieldDef
toLFieldDef f = LFieldDef {
        lFieldHaskell = fieldHaskell f
      , lFieldDB = fieldDB f
      , lFieldType = fieldType f
      , lFieldStrict = fieldStrict f
      , lFieldLabelAnnotations = labels
    }

    where
        labels = 
            let attrs = fieldAttrs f in
            List.foldl' (\acc attr -> 
                let ( prefix, affix) = Text.splitAt 9 attr in
                if acc /= Nothing && prefix /= "chevrons=" then
                    acc
                else
                    Just $ parseChevrons affix
              ) Nothing attrs

fieldTypeToType :: FieldType -> Type
fieldTypeToType ft = case ft of
    FTTypeCon Nothing con -> 
        ConT $ mkName $ Text.unpack con
    _ ->
        error "TODO: will this ever happen??"

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
                skipSpace
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
                    return $ LAConst name
                "Field" -> do
                    skipSpace
                    name <- takeAlphaNum
                    return $ LAField name
                _ -> 
                    fail $ "Unknown keyword `" ++ (Text.unpack constr) ++ "`in parseChevrons. Use `Id`, `Const`, or `Field`"
        
        takeAlphaNum = takeWhile1 Char.isAlphaNum
            









data LabelAnnotation = 
    LAId
  | LAConst Text
  | LAField Text
    deriving (Show, Eq, Read, Ord)


-- Or just use maybeRead? Or maybe not.
class Label l => LabelAnnotationConstant l where
    parseLAConstant :: String -> l
    parseLAConstant c = error $ "LabelAnnotationConstant is not defined for constant `" ++ c ++ "`"


-- EntityId -> String (FieldName) -> l



-- Example:
--   User
--       ident Text
--       password Text
--       email Text <Const Admin || Id, Id, _>
--       admin Bool
--   
--       UniqueEmail email
--       deriving Typeable
--

-- User defined:
-- instance LabelAnnotationConstant (DCLabel Principal) where
--     parseLAConstant "Admin" = toDCLabel [PrincipalAdmin] []
--     parseLAConstant _ = error "Undefined constant"
-- What about admin writes?? 
--      need conf/integrityPrincipalToLabel classes???

-- Generated:
-- 
-- -- mkLabelFieldChecks
-- readLabelUserEmail :: Entity User -> UserLabel
-- readLabelUserEmail (Entity uId _) = UserLabel
--     (UserLabelSet (Set.singleton uId))
--     (UserLabelSet Set.empty)
-- 
-- writeLabelUserEmail :: Entity User -> UserLabel
-- writeLabelUserEmail (Entity uId _) = UserLabel
--     (UserLabelSet Set.empty)
--     (UserLabelSet (Set.singleton uId))
-- 
-- createLabelUserEmail :: User -> UserLabel
-- createLabelUserEmail _ = bottom
-- 
-- -- mkLEntity -- TODO: how does this need to change??
-- 
-- -- mkProtected
-- data ProtectedUser = ProtectedUser {
--         pUserIdent :: Text
--       , pUserPassword :: Text
--       , pUserEmail :: Labeled (DCLabel Principal) Text
--       , pUserAdmin :: Bool
--     }
-- 
-- -- mkProtectedEntity
-- instance ProtectedEntity UserLabel User ProtectedUser where
--     toProtected user' = do
--         let (Entity uId user) = user'
--         let ident = userIdent user
--         let password = userPassword user
--         email <- toLabeledTCB (readUseremail user') $ do
--             taintLabel $ readUseremail user'
--             return $ userEmail user
--         let admin = userAdmin user
--         let protectedUser = ProtectedUser ident password email admin
--         return protectedUser
-- 





