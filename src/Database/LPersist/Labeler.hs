{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, OverloadedStrings, PatternGuards, FlexibleContexts, ScopedTypeVariables, TypeApplications #-}

module Database.LPersist.Labeler (mkLabels, mkLabels', mkLabelsWithDefault, mkLabelsWithDefault', phantomType, PhantomType) where

-- import Control.DeepSeq (force)
import Control.Monad
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..))
import qualified Data.Set as Set
import qualified Data.Text as Text
import Database.Persist.Types
import Language.Haskell.TH
-- import Language.Haskell.TH.Syntax (Lift(..))
import Prelude

import Database.LPersist
import Database.LPersist.TCB
import Internal
import LMonad.TCB
import PhantomType

-- | Functions that use TH to generate labeling code. 
-- All examples in this documentation reference the following Persist model:
--
--   User
--       ident Text
--       password Text
--       email Text <Const Admin || Id, Id>
--       admin Bool
--   
--       UniqueEmail email
--       deriving Typeable

mkLabels :: (ToLabel String l, ToLabel Lattice l, Label l)=> PhantomType l -> [EntityDef] -> Q [Dec]
mkLabels labelP ents = mkLabelsWithDefault labelP (LABottom, LATop) ents

mkLabelsWithDefault :: forall l . (ToLabel String l, ToLabel Lattice l, Label l) => PhantomType l -> (LabelAnnotation, LabelAnnotation) -> [EntityDef] -> Q [Dec]
mkLabelsWithDefault (PhantomType labelType) (defaultLabelL, defaultLabelR) ents = do
    let entsL = map (toLEntityDef defaultLabel) ents
    mapM_ (runInvariantChecks (Proxy @l)) entsL
    let labelFs' = concat $ map (mkLabelEntity' labelType) entsL
    let labelFs = concat $ map (mkLabelEntity labelType) entsL
    lEntityInstance <- mapM (mkLEntityInstance labelType) entsL
    protected <- mapM (mkProtectedEntity labelType) entsL
    protectedInstance <- mconcat <$> mapM (mkProtectedEntityInstance labelType) entsL
--    let serializedLEntityDef = mkSerializedLEntityDefs entsL
    return $ concat [ labelFs', labelFs, lEntityInstance, protected, protectedInstance] -- , serializedLEntityDef, concat labelFs,

    where
        defaultLabel = (canonicalLabelAnnotationOrder defaultLabelL, canonicalLabelAnnotationOrder defaultLabelR)

        -- mkLabelType = do
        --     exp <- lift labelP
        --     let pTyp = case exp of
        --           SigE _ proxyT -> proxyT
        --           _ -> error $ "Unknown label expression: " ++ show exp
        --     case pTyp of
        --         AppT (ConT p) t | p == ''Proxy ->
        --             return t
        --         _ ->
        --             error $ "Unknown label type: " ++ show pTyp

        -- labelType = 
        --     case Text.words $ Text.pack labelS of
        --         [] ->
        --             error $ "Label `" ++ labelS ++ "` not found"
        --         conT:rest ->
        --             if Text.length conT <= 1 || Char.isLower (Text.head conT) then
        --                 error $ "Invalid label type constructor `" ++ (Text.unpack conT) ++ "`"
        --             else
        --                 let con = ConT $ mkName $ Text.unpack conT in
        --                 List.foldl' (\acc typ -> AppT acc (ConT (mkName (Text.unpack typ)))) con rest



-- | Helper function that prints out the code generated at compilation.
mkLabels' :: (ToLabel String l, ToLabel Lattice l, Label l)=> PhantomType l -> [EntityDef] -> Q [Dec]
mkLabels' labelP ents = mkLabelsWithDefault' labelP (LABottom, LATop) ents

mkLabelsWithDefault' :: (ToLabel String l, ToLabel Lattice l, Label l) => PhantomType l -> (LabelAnnotation, LabelAnnotation) -> [EntityDef] -> Q [Dec]
mkLabelsWithDefault' labelP defaultLabel ents = do
    labels <- mkLabelsWithDefault labelP defaultLabel ents
    fail $ show $ pprint labels


-- | Create protected ADTs for the models in Persist's DSL. 
-- Ex: ProtectedUser is created for protected version of User.
--
-- data ProtectedUser = ProtectedUser {
--         pUserIdent :: Text
--       , pUserPassword :: Text
--       , pUserEmail :: Labeled (DCLabel Principal) Text
--       , pUserAdmin :: Bool
--     }

mkProtectedEntity :: Type -> LEntityDef -> Q Dec
mkProtectedEntity labelType ent =
    let pFields = map mkProtectedField (Map.elems $ lEntityFields ent) in
    return $ DataD [] pName [] Nothing [RecC pName pFields] []

    where
        eName = lEntityHaskell ent
        pName = mkName $ "Protected" ++ eName
        mkProtectedField field = 
            let fName = mkName $ 'p':(eName ++ (headToUpper (lFieldHaskell field))) in
            let strict = Bang NoSourceUnpackedness $ if lFieldStrict field then SourceStrict else NoSourceStrictness in
            let rawType = fieldTypeToType $ lFieldType field in
            let typ = 
                  -- If the field's label equals the table label, we don't need to wrap it in a Labeled.
                  if not (isFieldLabeled ent field) then
                    rawType
                  else
                    AppT (AppT (ConT (mkName "Labeled")) labelType) rawType
            in
            (fName, strict, typ)



-- | Create LEntity instance for a given entity. Joins all field label calls
-- Ex:
--
-- instance LEntity (DCLabel Principal) User where
--     getFieldLabels _e = [bottom, labelUserUser _e]
--     tableLabel Proxy = bottom
--
--     getDependencyLabelsLabels Proxy =
--         [labelUserIdNId']
--     
--     canAllocProtected (Protected k (PEntity (Labeled lUser user) ...)) = do
--         c <- getClearance
--         -- lc <- getCurrentLabel
--         let _e = Entity K (User user) ... in
--         let _labelUserId' = labelUserId _e in
--         return $ k? && lUser `canFlowTo` labelUserId' && labelUserId' `canFlowTo` c && ...

--
--
--     getReadLabels _e = 
--         [bottom, readLabelUserUser _e]
--     getLabelWrite _e =
--         [bottom, writeLabelUserUser _e]
--     getLabelCreate _e =
--         [bottom, createLabelUserUser _e]

mkLEntityInstance :: Type -> LEntityDef -> Q Dec
mkLEntityInstance labelType ent = --, createLabels)) = 

    -- let (readLabels, writeLabels) = lEntityUniqueFieldLabelsAnnotations ent in
    -- let rExpr = ListE $ map (mkExpr lFieldReadLabelName) readLabels in
    -- let wExpr = ListE $ map (mkExpr lFieldWriteLabelName) writeLabels in
    -- let cExpr = ListE $ map (mkExpr lFieldCreateLabelName) createLabels in

    let fLabels = lEntityUniqueFieldLabelsAnnotations ent in
    let fExpr = ListE $ map mkExpr fLabels in

    let tLabel = lEntityLabelAnnotations ent in
    -- Enforce invariant that tLabel is constant.
    -- JP: This check is redundant (See `toLEntityDef` in Internal.hs).
    let tExpr = if isLabelAnnotationConstant (fst tLabel) && isLabelAnnotationConstant (snd tLabel) then
            mkExprConst tLabel 
          else
            error $ "mkLEntityInstance: Table label must be constant (" ++ eName ++ ")."
    in

    let pat = VarP e in
    let funcs = [
            FunD 'getFieldLabels [Clause [pat] (NormalB fExpr) []]
          , FunD 'tableLabel [Clause [ConP 'Proxy []] (NormalB tExpr) []]
          , getDependencyLabelsLabelsF
          --   FunD (mkName "getReadLabels") [Clause [pat] (NormalB rExpr) []]
          -- , FunD (mkName "getWriteLabels") [Clause [pat] (NormalB wExpr) []]
          -- , FunD (mkName "getCreateLabels") [Clause [pat] (NormalB cExpr) []]
          ]
    in
    return $ InstanceD Nothing [] (AppT (AppT (ConT ''LEntity) labelType) (ConT (mkName eName))) funcs

    where
        eName = lEntityHaskell ent
        e = mkName "_entity"

        canFlowToE e1 e2 = AppE (AppE (VarE 'canFlowTo) e1) e2
        andE e1 e2 = AppE (AppE (VarE (mkName "&&")) e1) e2

        canAllocProtectedF = 
            let pattern = protectedEntityPattern ent in
            let cV = mkName "_cc" in
            let eV = mkName "_e" in
            let fJust _ x Nothing = Just x 
                fJust f x (Just t) = Just (f x t)
            in
            let checkE = 
                  let me = foldr (\field acc -> 
                          -- Skip if unprotected field. This is safe since we know lc `canFlowTo` l_T `canFlowTo` cc from previous check.
                          case labeledVarNames ent field of
                            Left _ ->
                                acc
                            Right ( lName, vName) ->
                                let ltfiV = mkName $ "_ltfi" ++ eName ++ fName in
                                let a = andE 
                                        (canFlowToE (VarE liV) (VarE ltfiV))
                                        (canFlowTo (VarE ltfiV) (VarE cV)) 
                                in
                                let labelFName = lFieldLabelName eName $ lFieldLabelAnnotations field in
                                let x = LetE [ValD (VarP ltfiV) (NormalB $ AppE (VarE labelFName) (VarE eV)) []] a in
                                fJust (\e acc -> andE acc e) x 
                        ) Nothing (lEntityFields ent) 
                  in
                  case me of
                    Nothing -> ConE 'True
                    Just cE -> 
                        let tE = foldr (\field acc -> AppE acc (VarE (fieldVarName eName fName)) (ConE (mkName eName))) $ lEntityFields ent in
                        LetE [ValD (VarP eV) (NormalB $ AppE (AppE (ConE 'Entity) (VarE "_key")) tE) []] cE
            in
            let body = DoE [
                    BindS (VarP $ cV) (VarE 'getClearance)
                  , NoBindS (AppE (VarE 'return) checkE)
                  ]
            in
            FunD 'canAllocProtected [Clause [pattern] (NormalB body) []]

        getDependencyLabelsLabelsF = 
            let fieldNameToLabelName name = lFieldLabelName' eName $ lFieldLabelAnnotations $ getLEntityFieldOrIdDef ent name in
            let body = ListE $ fmap (VarE . fieldNameToLabelName) $ Set.toList $ lEntityDependencyFields ent in
            FunD 'getDependencyLabelsLabels [Clause [ConP 'Proxy []] (NormalB body) []]

        -- mkExpr (read, write) = 
        --     let r = mkLabel read in
        --     let w = mkLabel write in
        --     -- JP: Join to combine left and right halves?..
        --     AppE (AppE (VarE 'lub) r) w

        mkExpr anns = 
            let fName = lFieldLabelName eName anns in
            AppE (VarE fName) (VarE e)

        mkExprConst anns = 
            let fName = lFieldLabelName' eName anns in
            VarE fName

        -- mkExpr nameF anns = 
        --     let fName = nameF eName anns in
        --     List.foldl' (\acc ann -> case ann of
        --         LAId -> 
        --             AppE acc (VarE eId)
        --         LAConst _ ->
        --             acc
        --         LAField f ->
        --             let name = mkName $ headToLower eName ++ headToUpper f in
        --             AppE acc (AppE (VarE name) (VarE e))
        --       ) (VarE fName) anns

protectedEntityPattern :: LEntityDef -> Pat
protectedEntityPattern ent = 
    ConP 'PEntity [ VarE "_key", protectedPattern ent]

protectedPattern :: LEntityDef -> Pat
protectedPattern ent =
    let fieldPs = map (\field -> case labeledVarNames ent field of
            Left lName -> VarP lName
            Right (lName, vName) -> ConP 'Labeled [VarP lName, vName]
          ) $ Map.elems $ lEntityFields ent
    in
    ConP pName fieldPs

    where
        eName = lEntityHaskell ent
        pName = mkName $ "Protected" ++ eName

fieldVarName :: String -> String -> Name
fieldVarName eName fName = mkName $ "_v" ++ eName ++ fName

labeledVarNames :: LEntityDef -> LFieldDef -> Either Name (Name, Name)
labeledVarNames ent field = 
    let vName = fieldVarName eName fName in
    if isFieldLabeled ent field then
        let lName = mkName $ "_l" ++ eName ++ fName in
        Right (lName, vName)
    else
        Left vName

{-
-- | Create LEntity instance for a given entity. Joins all field label calls
-- Ex:
--
-- instance LEntity (DCLabel Principal) User where
--     getLabelRead _e = 
--         readLabelUserEmail _e
--     getLabelWrite _e =
--         writeLabelUserEmail _e
--     getLabelCreate _e =
--         createLabelUserEmail _e

mkLEntityInstance :: Type -> LEntityDef -> Q Dec
mkLEntityInstance labelType ent = 
    let expr = List.foldl' mkStmts Nothing (lEntityFields ent) in
    let (rExpr, wExpr, cExpr) = case expr of 
          Nothing ->
            ( bottom, bottom, bottom)
          Just exprs ->
            exprs
    in
    let funcs = [
            FunD (mkName "getLabelRead") [Clause [VarP e] (NormalB rExpr) []],
            FunD (mkName "getLabelWrite") [Clause [VarP e] (NormalB wExpr) []],
            FunD (mkName "getLabelCreate") [Clause [VarP e] (NormalB cExpr) []]
          ]
    in
    return $ InstanceD [] (AppT (AppT (ConT (mkName "LEntity")) labelType) (ConT (mkName eName))) funcs

    where
        eName = lEntityHaskell ent
        e = mkName "_e"
        bottom = VarE $ mkName "bottom"
        appJoin = AppE . (AppE (VarE (mkName "lub")))
        mkStmts acc field = case lFieldLabelAnnotations field of
            Nothing -> 
                acc
            _ -> 
                let baseName = eName ++ (headToUpper (lFieldHaskell field)) in
                let rExpr = AppE (VarE (mkName ("readLabel"++baseName))) (VarE e) in
                let wExpr = AppE (VarE (mkName ("writeLabel"++baseName))) (VarE e) in
                let cExpr = AppE (VarE (mkName ("createLabel"++baseName))) (VarE e) in
                Just $ case acc of
                    Nothing ->
                        ( rExpr, wExpr, cExpr)
                    Just (rAcc, wAcc, cAcc) ->
                        ( appJoin rExpr rAcc, appJoin wExpr wAcc, appJoin cExpr cAcc)
-}

isLabelAnnotationConstant :: LabelAnnotation -> Bool
isLabelAnnotationConstant = helper True
    where
        helper False _ = False
        helper True LAId = False
        helper True (LAField _) = False
        helper True LATop = True
        helper True LABottom = True
        helper True (LAConst _) = True
        helper True (LAJoin a b) = helper True a && helper True b
        helper True (LAMeet a b) = helper True a && helper True b

-- | Creates functions that get labels for each field in an entity. 
-- Ex:
--
-- labelUserAdminGLBId :: Entity User -> DCLabel Principal
-- labelUserAdminGLBId (Entity _eId _entity) =
--     labelUserAdminGLBId' _eId
--

mkLabelEntity :: Type -> LEntityDef -> [Dec]
mkLabelEntity labelType ent = -- , createLabels)) = 
    let labels = lEntityUniqueFieldLabelsAnnotations ent in

    let labelsD = map mkLabelField labels in

    concat labelsD



    -- let (readLabels, writeLabels) = lEntityUniqueFieldLabelsAnnotations ent in
    -- let readD = map (mkLabelField lFieldReadLabelName lFieldReadLabelName') readLabels in
    -- let writeD = map (mkLabelField lFieldWriteLabelName lFieldWriteLabelName') writeLabels in
    -- -- let createD = map (mkLabelField' lFieldCreateLabelName lFieldCreateLabelName') createLabels in
    -- concat $ readD ++ writeD -- ++ createD
    
    where
        eName = lEntityHaskell ent
        eId = mkName "_eId"
        e = mkName "_entity"
        typ = AppT (AppT ArrowT (AppT (ConT (mkName "Entity")) (ConT (mkName eName)))) labelType
        -- typ' = AppT (AppT ArrowT (ConT (mkName eName))) labelType

        mkBody anns = 
            let args = lFieldLabelArguments anns in
            let helper annotation acc = case annotation of
                  LAConst _ ->
                    acc
                  LAId ->
                    AppE acc (VarE eId)
                  LAField s ->
                    let f = mkName $ headToLower eName ++ headToUpper s in
                    AppE acc (AppE (VarE f) (VarE e))
                  LAMeet _ _ ->
                    acc
                  LAJoin _ _ ->
                    acc
                  LABottom ->
                    acc
                  LATop ->
                    acc
                  
            in
            foldr helper (VarE $ lFieldLabelName' eName anns) args

        -- mkLabelField' anns = 
        --     let name = lFieldLabelName eName anns in
        --     let sig = SigD name typ' in
        --     let def = FunD name [Clause [VarP e] (NormalB $ mkBody anns) []] in
        --     [sig, def]

        mkLabelField anns = 
            let name = lFieldLabelName eName anns in
            let sig = SigD name typ in
            let def = FunD name [Clause [ConP 'Entity [VarP eId, VarP e]] (NormalB $ mkBody anns) []] in
            [sig, def]

-- | Similar to mkLabelEntity, except this function creates code that returns the labels given what the label depends on instead of the entire entity. 
-- Ex:
--
-- labelAdminGLBId' :: UserId -> DCLabel Principal
-- labelAdminGLBId' uId = 
--     ((toConfidentialityLabel "Admin") `glb` (toConfidentialityLabel uId))
--
mkLabelEntity' :: Type -> LEntityDef -> [Dec]
mkLabelEntity' labelType ent = -- , createLabels)) = 
    let labels' = lEntityUniqueFieldLabelsAnnotations ent in

    -- Add table label if it doesn't exist.
    -- JP: Should use a Set.
    let tLabel = lEntityLabelAnnotations ent in
    let labels'' = if List.elem tLabel labels' then
            labels'
          else
            tLabel:labels'
    in

    let labelsD' = map mkLabelField' labels'' in

    concat labelsD'

--     let (readLabels, writeLabels) = lEntityUniqueFieldLabelsAnnotations ent in
--     let readD = map (mkLabelField' lFieldReadLabelName' toConfLabel) readLabels in
--     let writeD = map (mkLabelField' lFieldWriteLabelName' toIntegLabel) writeLabels in
--     -- let createD = map (mkLabelField' lFieldCreateLabelName' toIntegLabel) createLabels in
--     concat $ readD ++ writeD -- ++ createD

    where
        eName = lEntityHaskell ent
        toConfLabel = VarE $ mkName "toConfidentialityLabel"
        toIntegLabel = VarE $ mkName "toIntegrityLabel"
        -- bottom = VarE $ mkName "bottom"
        appMeet = AppE . (AppE (VarE (mkName "glb")))
        appJoin = AppE . (AppE (VarE (mkName "lub")))

        mkType =
            let helper annotation acc = case annotation of
                  LAConst _ ->
                    acc
                  LAId ->
                    let name = mkName $ eName ++ "Id" in
                    AppT (AppT ArrowT (ConT name)) acc
                  LAField s -> 
                    let typ = getLEntityFieldType ent s in
                    AppT (AppT ArrowT typ) acc
                  LABottom ->
                    acc
                  LATop ->
                    acc
                  LAJoin _ _ ->
                    acc
                  LAMeet _ _ ->
                    acc
            in
            List.foldr helper labelType

        mkPattern = 
            let helper annotation acc = case annotation of
                  LAConst _ ->
                    acc
                  LAId -> 
                    (VarP $ mkName "_id"):acc
                  LAField s ->
                    (VarP $ mkName $ "_" ++ s):acc
                  LABottom ->
                    acc
                  LATop ->
                    acc
                  LAJoin _ _ ->
                    acc
                  LAMeet _ _ ->
                    acc
            in
            List.foldr helper []

        mkBody (c, i) = 
            let appF f ann = case ann of
                  LAId ->
                    AppE f $ VarE $ mkName "_id"
                  LAConst c ->
                    AppE f $ SigE (LitE $ StringL c) $ ConT $ mkName "String"
                  LAField fName ->
                    AppE f $ VarE $ mkName $ "_" ++ fName
                  LAJoin a b ->
                    appJoin (appF f a) $ appF f b
                  LAMeet a b ->
                    appMeet (appF f a) $ appF f b
                  LABottom ->
                    AppE f $ ConE 'Bottom
                  LATop ->
                    AppE f $ ConE 'Top
            in
            appJoin (appF toConfLabel c) (appF toIntegLabel i)
            -- List.foldl' (\acc ann -> appMeet acc $ appF ann) (appF h) t

        mkLabelField' anns = 
            let args = lFieldLabelArguments anns in
            let name = lFieldLabelName' eName anns in
            let sig = SigD name $ mkType args in
            let def = FunD name [Clause (mkPattern args) (NormalB $ mkBody anns) []] in
            [sig, def]

{-
-- | Similar to mkLabelEntity, except this function creates code that returns the labels given what the label depends on instead of the entire entity. 
-- Ex:
--
-- readLabelUserEmail' :: UserId -> DCLabel Principal
-- readLabelUserEmail' uId = 
--     ((toConfidentialityLabel "Admin") `glb` (toConfidentialityLabel uId))
--
-- writeLabelUserEmail' :: UserId -> DCLabel Principal
-- writeLabelUserEmail' uId = 
--     (toIntegrityLabel uId)
--
-- createLabelUserEmail' :: DCLabel Principal
-- createLabelUserEmail' = 
--     bottom

mkLabelEntity' :: Type -> LEntityDef -> [Dec]
mkLabelEntity' labelType ent =
    let labelFs = map mkLabelField' (Map.elems $ lEntityFields ent) in
    concat labelFs

    where
        eName = lEntityHaskell ent
        toConfLabel = VarE $ mkName "toConfidentialityLabel"
        toIntegLabel = VarE $ mkName "toIntegrityLabel"
        bottom = VarE $ mkName "bottom"
        appMeet = AppE . (AppE (VarE (mkName "glb")))
        mkType =
            let helper annotation acc = case annotation of
                  LAConst _ ->
                    acc
                  LAId ->
                    let name = mkName $ eName ++ "Id" in
                    AppT (AppT ArrowT (ConT name)) acc
                  LAField s -> 
                    let typ = getLEntityFieldType ent s in
                    AppT (AppT ArrowT typ) acc
            in
            List.foldr helper labelType
        mkPattern = 
            let helper annotation acc = case annotation of
                  LAConst _ ->
                    acc
                  LAId -> 
                    (VarP $ mkName "_id"):acc
                  LAField s ->
                    (VarP $ mkName $ "_" ++ s):acc
            in
            List.foldr helper []
        mkBody f anns = case anns of
            [] -> 
                bottom
            h:t -> 
                let appF ann = case ann of
                      LAId ->
                        AppE f $ VarE $ mkName "_id"
                      LAConst c ->
                        AppE f $ SigE (LitE $ StringL c) $ ConT $ mkName "String"
                      LAField fName ->
                        AppE f $ VarE $ mkName $ "_" ++ fName
                in
                List.foldl' (\acc ann -> appMeet acc $ appF ann) (appF h) t
        mkLabelField' field = 
            let ( readAnns, writeAnns, createAnns) = lFieldLabelAnnotations field in
            let baseName = eName ++ (headToUpper (lFieldHaskell field)) in
            let readName = mkName $ "readLabel" ++ baseName ++ "'" in
            let writeName = mkName $ "writeLabel" ++ baseName ++ "'" in
            let createName = mkName $ "createLabel" ++ baseName ++ "'" in
            let readSig = SigD readName $ mkType readAnns in
            let readDef = FunD readName [Clause (mkPattern readAnns) (NormalB $ mkBody toConfLabel readAnns) []] in
            let writeSig = SigD writeName $ mkType writeAnns in
            let writeDef = FunD writeName [Clause (mkPattern writeAnns) (NormalB $ mkBody toIntegLabel writeAnns) []] in
            let createSig = SigD createName $ mkType createAnns in
            let createDef = FunD createName [Clause (mkPattern createAnns) (NormalB $ mkBody toIntegLabel createAnns) []] in
            [readSig, readDef, writeSig, writeDef, createSig, createDef]
-}

-- | Create ProtectedEntity instance for given entity.
-- Ex:
--
-- instance ProtectedEntity (DCLabel Principal) User ProtectedUser where
--     toProtected _entity@(Entity _eId _e) =
--         let _labelUserAdminGLBemail = labelUserAdminGLBemail
--         let ident = userIdent _e
--         let password = userPassword _e
--         let email = Labeled _labelUserAdminGLBemail (userEmail _e)
--         let admin = userAdmin _e
--         ProtectedUser ident password email admin

mkProtectedEntityInstance :: Type -> LEntityDef -> Q [Dec]
mkProtectedEntityInstance labelType ent = do
    let labels = lEntityUniqueFieldLabelsAnnotations ent
    let lStmts acc = foldr mkLabelStmts acc labels
    ( fStmts, fExps) <- foldM (mkProtectedFieldInstance ent) (id,[]) $ lEntityFields ent
    let recordCons = RecConE (mkName pName) fExps
    let body = lStmts $ fStmts recordCons
    let toProtected = FunD 'toProtectedTCB [Clause [AsP entity (ConP (mkName "Entity") [VarP eId,VarP e])] (NormalB body) []]
    let inst = InstanceD Nothing [] (AppT (AppT (ConT ''ProtectedEntity) labelType) (ConT (mkName eName))) [toProtected]
    let typInst = TySynInstD ''Protected $ TySynEqn [ConT (mkName eName)] (ConT $ mkName pName)
    return [inst, typInst]

    where
        eName = lEntityHaskell ent
        pName = "Protected" ++ eName
        e = mkName "_e"
        eId = mkName "_eId"
        entity = mkName "_entity"

        -- Skip labels that match table label.
        mkLabelStmts anns acc | anns == lEntityLabelAnnotations ent = 
            acc
        mkLabelStmts anns acc = 
            let vName = lFieldLabelVarName eName anns in
            let fName = lFieldLabelName eName anns in
            LetE [ValD (VarP vName) (NormalB (AppE (VarE fName) (VarE entity))) []] acc

        mkProtectedFieldInstance :: LEntityDef -> (Exp -> Exp, [FieldExp]) -> LFieldDef -> Q (Exp -> Exp, [FieldExp])
        mkProtectedFieldInstance ent (sAcc, fAcc) field = do
            let fName = lFieldHaskell field
            let getter = mkName $ (headToLower eName) ++ (headToUpper fName)
            vName <- newName "v"
            let setter = mkName $ 'p':(eName ++ (headToUpper fName))
            let newF = (setter, VarE vName)
            let newS acc = sAcc $ if isFieldLabeled ent field then
                    let anns = lFieldLabelAnnotations field in
                    let lName = lFieldLabelVarName eName anns in
                    LetE [ValD (VarP vName) (NormalB (AppE (AppE (ConE 'Labeled) (VarE lName)) (AppE (VarE getter) (VarE e)))) []] acc
                  else
                    LetE [ValD (VarP vName) (NormalB (AppE (VarE getter) (VarE e))) []] acc
            return ( newS, (newF:fAcc))

fieldTypeToType :: FieldType -> Type
fieldTypeToType (FTTypeCon Nothing con) = 
    ConT $ mkName $ Text.unpack con
fieldTypeToType (FTTypeCon (Just mod) con) = 
    ConT $ mkName $ (Text.unpack mod) ++ "." ++ Text.unpack con
fieldTypeToType (FTApp f x) = 
    AppT (fieldTypeToType f) (fieldTypeToType x)
fieldTypeToType (FTList x) = 
    AppT ListT $ fieldTypeToType x

getLEntityFieldType :: LEntityDef -> String -> Type
getLEntityFieldType ent fName = 
    let def = getLEntityFieldDef ent fName in
    fieldTypeToType $ lFieldType $ def


-- Run checks that each dependent label can flow to the table label.
-- invariantUser = 
--     let tl = tableLabel (Proxy :: Proxy User) in
--     ()
--
--     if userADependentFieldLabel' `canFlowTo` tl then
--         ... 
--         ()
--     else
--         error "Field ADependentField's label must flow to the table label since it is a dependency."
--     
runInvariantChecks :: forall l . (ToLabel String l, Label l, ToLabel Lattice l) => Proxy l -> LEntityDef -> Q ()
runInvariantChecks Proxy ent = 
    mapM_ invariantCheck $ lEntityDependencyFields ent

    where
        tableLabel :: l
        tableLabel = toConstantLabel $ lEntityLabelAnnotations ent

        -- Check if this is a dependency field.
        invariantCheck name = do
            let field = getLEntityFieldOrIdDef ent name

            -- Check that field is constant.
            let la@(c, i) = lFieldLabelAnnotations field
            unless (isConstantLabel c && isConstantLabel i) $
                error $ "Label for field `" ++ name ++ "` is not constant."

            -- Check that field label can flow to table label.
            let fieldLabel = toConstantLabel la
            unless (fieldLabel `canFlowTo` tableLabel) $
                error $ "Label for field `" ++ name ++ "` does not flow to the table label."



--     let typ = SigD fName (ConT ''())
--     body <- mkBody
--     let fun = FunD fName [Clause [] body []]
-- 
--     return $ [typ, fun]
-- 
--     where
--         dependencyCheck acc fieldS | Just field <- Map.lookup fieldS (lEntityFields ent) = 
--             -- Check that field is constant.
--             let la@(c,i) = lFieldLabelAnnotations field in
--             if not (isLabelAnnotationConstant c && isLabelAnnotationConstant i) then
--                 error $ "The label of field `" ++ fieldS ++ "` of entity `" ++ eS ++ "` must be constant"
--             else
--                 let fName' = lFieldLabelName' (lEntityHaskell ent) la in
--                 return $ CondE 
--                     (AppE (AppE (VarE 'canFlowTo) (VarE fName')) (VarE tlName)) 
--                     acc 
--                     (AppE (VarE 'error) (LitE $ StringL $ "The label of field `" ++ fieldS ++ "` of entity `" ++ eS ++ "` must flow to the table label"))
--         dependencyCheck _ fieldS = error $ "Could not find field `" ++ fieldS ++ "`"
-- 
--         mkBody = do
--           conditions <- foldM dependencyCheck (TupE []) $ lEntityDependencyFields ent
--           return $ NormalB $ 
--             AppE (VarE 'force) $
--             LetE [ValD (VarP tlName) (NormalB (SigE (AppE (VarE 'tableLabel) (SigE (ConE 'Proxy) (AppT (ConT ''Proxy) (ConT (eName))))) labelType)) []] $
--             conditions
-- 
--         eS = lEntityHaskell ent
--         eName = mkName eS
--         fName = mkName $ "invariant" ++ eS
--         tlName = mkName "tl"

-- -- We can derive this in 8.0.1
-- instance Lift LabelAnnotation where
--     lift LABottom = conE 'LABottom
--     lift LATop = conE 'LATop
--     lift LAId = conE 'LAId
--     lift (LAConst s) = appE (conE 'LAConst) (lift s)
--     lift (LAField s) = appE (conE 'LAField) (lift s)
--     lift (LAMeet a b) = appE (appE (conE 'LAMeet) (lift a)) (lift b)
--     lift (LAJoin a b) = appE (appE (conE 'LAJoin) (lift a)) (lift b)

isConstantLabel :: LabelAnnotation -> Bool
isConstantLabel LABottom = True
isConstantLabel LATop = True
isConstantLabel LAId = False
isConstantLabel (LAConst _) = True
isConstantLabel (LAField _) = False
isConstantLabel (LAMeet a b) = isConstantLabel a && isConstantLabel b
isConstantLabel (LAJoin a b) = isConstantLabel a && isConstantLabel b

toConstantLabel :: (Label l, ToLabel String l, ToLabel Lattice l) => (LabelAnnotation, LabelAnnotation) -> l
toConstantLabel l@(c,i) = helper toConfidentialityLabel c `lub` helper toIntegrityLabel i

    where
        helper :: (Label l, ToLabel String l, ToLabel Lattice l) => (forall s . ToLabel s l => s -> l) -> LabelAnnotation -> l
        helper _ LAId = error $ "Not a constant label: " ++ show l
        helper _ (LAField _) = error $ "Not a constant label: " ++ show l
        helper f (LAConst s) = f s
        helper f LABottom = f Bottom
        helper f LATop = f Top
        helper f (LAMeet a b) = helper f a `glb` helper f b
        helper f (LAJoin a b) = helper f a `lub` helper f b

