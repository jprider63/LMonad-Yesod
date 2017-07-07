{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Database.LEsqueleto (mkLSql, module Export) where

import Control.Monad.Trans.Class (lift)
import Data.Attoparsec.Text (parseOnly)
import qualified Data.Char as Char
import qualified Data.List as List
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map
import qualified Database.Esqueleto as Esq
import Database.Esqueleto as Export (Value(..), SqlBackend)
import Data.Maybe (isJust)
import Database.Persist.Types
import Data.Text (Text)
import qualified Data.Text as Text
import Database.LPersist
--import Database.Persist
import qualified Language.Haskell.Meta.Parse as Meta
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import LMonad.TCB

import Database.LEsqueleto.LSql
import Internal

-- | Generate the quasiquoter function `lsql` that parses the esqueleto DSL.
mkLSql :: [EntityDef] -> Q [Dec]
mkLSql ents' = 
    let lsql = mkName "lsql" in
    let sig = SigD lsql (ConT ''QuasiQuoter) in
    let ents = mkSerializedLEntityDefs $ map toLEntityDef ents' in
    let def = ValD (VarP lsql) (NormalB (AppE (VarE 'lsqlHelper) ents)) [] in
    let lsql' = mkName "lsql'" in
    let sig' = SigD lsql' (ConT ''QuasiQuoter) in
    let def' = ValD (VarP lsql') (NormalB (AppE (VarE 'lsqlHelper') ents)) [] in
    return [ sig, def, sig', def']

-- | Serialize LEntityDefs so that lsql can access them in other modules. 
-- Ex:
--
-- [ LEntityDef "User" [LFieldDef "ident" (FTTypeCon "Text") True Nothing, ...], ...]
mkSerializedLEntityDefs :: [LEntityDef] -> Exp
mkSerializedLEntityDefs ents' = 
    ListE $ List.map mkSerializedLEntityDef ents'

    where
        mkSerializedLEntityDef ent = 
            let str = LitE $ StringL $ lEntityHaskell ent in
            let fields = mkSerializedLFieldsDef $ Map.elems $ lEntityFields ent in
            -- if (lEntityHaskell ent) == "User" then
            --     error $ show ent
            -- else
            AppE (AppE (ConE 'LEntityDef) str) fields

        mkSerializedText t = SigE (LitE $ StringL $ Text.unpack t) (ConT ''Text)
        mkSerializedFieldType typ = case typ of
            FTTypeCon moduleM' name' ->
                let moduleM = maybe (ConE 'Nothing) (\m -> AppE (ConE 'Just) (mkSerializedText m)) moduleM' in
                let name = mkSerializedText name' in
                AppE (AppE (ConE 'FTTypeCon) moduleM) name
            FTApp typ1' typ2' ->
                let typ1 = mkSerializedFieldType typ1' in
                let typ2 = mkSerializedFieldType typ2' in
                AppE (AppE (ConE 'FTApp) typ1) typ2
            FTList typ' ->
                let typ = mkSerializedFieldType typ' in
                AppE (ConE 'FTList) typ
        mkSerializedLabelAnnotation la = case la of 
            LAId ->
                ConE 'LAId
            LAConst s ->
                AppE (ConE 'LAConst) (LitE $ StringL s)
            LAField s -> 
                AppE (ConE 'LAField) (LitE $ StringL s)
        mkSerializedLFieldsDef fields' = 
            let helper field = 
                  let name = LitE $ StringL $ lFieldHaskell field in
                  let typ = mkSerializedFieldType $ lFieldType field in
                  let strict = ConE $ if lFieldStrict field then 'True else 'False in
                  let anns = 
                        let ( r', w') = lFieldLabelAnnotations field in
                        let r = ListE $ map mkSerializedLabelAnnotation r' in
                        let w = ListE $ map mkSerializedLabelAnnotation w' in
                        -- let c = ListE $ map mkSerializedLabelAnnotation c' in
                        TupE [ r, w] -- , c]
                  in
                  let def = AppE (AppE (AppE (AppE (ConE 'LFieldDef) name) typ) strict) anns in
                  TupE [name, def]
            in
            -- ListE $ map helper fields'
            AppE (VarE 'Map.fromList) $ ListE $ map helper fields'

lsqlHelper :: [LEntityDef] -> QuasiQuoter
lsqlHelper ents = QuasiQuoter {
        quoteExp = (generateSql ents) . Text.pack
    }

generateSql :: [LEntityDef] -> Text -> Q Exp
generateSql lEntityDefs s = 
    -- Parse the DSL. 
    let ast = case parseOnly parseCommand s of
          Left err ->
            error $ "Error parsing lsql statement: " ++ err
          Right res ->
            res
    in
    let normalized = normalizeTerms ast in
    let isTableOptional tableS =
          let createAssoc (Tables ts join table _) lvl' =
                let ( lvl, next, prev) = case join of
                      LeftOuterJoin ->
                        if lvl' == 0 then 
                            ( 1, 0, 1)
                        else
                            ( lvl', lvl' - 1, 0)
                      InnerJoin ->
                        ( lvl', lvl', 0)
                      RightOuterJoin -> 
                        ( lvl', lvl' + 1, 0)
                      FullOuterJoin ->
                        ( lvl' + 1, lvl' + 1, 1)
                in
                let ( mapping, correction) = createAssoc ts next in
                ( ( table, lvl + correction):mapping, prev + correction)
              createAssoc (Table table) lvl = 
                ( [( table, lvl)], 0)
          in
          let ( mapping, _) = createAssoc (commandTables normalized) (0 :: Int) in
          -- error $ show mapping
          maybe 
            (error $ "Could not find table `" ++ tableS ++ "`") 
            (> 0) 
            $ List.lookup tableS mapping
    in
    let protected = (commandSelect normalized) == PSelect in
    let terms = reqTermsCommand isTableOptional normalized in 
    -- TODO: Add some check that all the table and field names used match up with existing things?? XXX
    {-
    ...
    normalize terms
    make map from tables -> isMaybe??
    get all terms
    get all dependency terms (or a map of terms -> [dependencies]??)
    union terms and dependency terms
    generate sql query
    generate map over results
        taintlabel or tolabeled result
        return terms

    ...
    need to check if fields/tables are maybes???
    -}
    do
    -- error $ show ast
    res <- newName "res"
    let query = 
          let tables = commandTables normalized in
          let returns = AppE (VarE 'return) $ TupE $ List.map (\rterm -> case rterm of
                    ReqField table field _ _ ->
                        mkExprTF (isTableOptional table) table field -- TODO: Does the option matter here??? XXX
                        -- mkExprTF False table field -- TODO: Does the option matter here??? XXX
                    ReqEntity ent _ _ -> 
                        VarE $ varNameTable ent
                ) terms 
          in
          BindS (VarP res) $ AppE (VarE 'Esq.select) $ AppE (VarE 'Esq.from) $ 
            LamE [mkQueryPatternTables tables] $ DoE 
                ((mkOnTables isTableOptional tables) 
                ++ (mkWhere isTableOptional $ commandWhere normalized) 
                ++ (mkOrderBy isTableOptional $ commandOrderBy normalized)
                ++ (mkLimit $ commandLimit normalized)
                ++ (mkOffset $ commandOffset normalized)
                ++ [NoBindS returns])
    let taint = 
          let fun = 
                let pat = TupP $ List.map ( \rterm -> 
                        let constr = case rterm of
                              ReqField table field _ _ ->
                                ConP 'Value [VarP $ varNameTableField table field]
                              ReqEntity table optional _ ->
                                if optional then
                                    VarP $ varNameTableField table "maybe"
                                else
                                    mkEntityPattern table
                        in
                        constr
                      ) terms 
                in
                let body = 
                      let getExpr table field = 
                            let res = List.foldl' ( \acc rterm -> maybe ( case rterm of 
                                    ReqField table' field' _ _
                                      | table == table' && field == field' ->
                                        Just $ VarE $ varNameTableField table field
                                    ReqEntity table' optional _
                                      | table == table' ->
                                        -- TODO: implement this FIXME XXX
                                        let expr = 
                                              if field == "id" then
                                                VarE $ varNameTableField table field
                                              else
                                                let getter = mkName $ (headToLower table) ++ (headToUpper field) in
                                                AppE (VarE getter) $ VarE $ varNameTable table
                                        in
                                        Just $ if optional then
                                            -- Here we assert that the field is not Nothing, since we know that is optional. Warning: If this assertion is wrong, things could fail at runtime. 
                                            let body = AppE (VarE 'fromJust) (VarE $ varNameTableField table' "maybe") in
                                            LetE [ValD (mkEntityPattern table') (NormalB body) []] expr
                                        else
                                            expr
                                    _ ->
                                        acc
                                    
                                  ) Just acc ) Nothing terms 
                            in
                            maybe (error $ "Could not find expression for table `"++table++"` and field `"++field++"`") id res
                      in
                      let taints = List.foldr (\rterm acc -> case rterm of
                                ReqField _ _ False _ -> 
                                    acc
                                ReqField _ _ _ Nothing -> 
                                    acc
                                ReqField table field _returning (Just deps) -> 
                                    let labeler = VarE $ mkName $ "readLabel" ++ (headToUpper table) ++ (headToUpper field) ++ "'" in
                                    let label = List.foldl' (\acc (table',field') -> 
                                            AppE acc $ getExpr table' field'
                                          ) labeler deps
                                    in
                                    let stmt = if protected then
                                            let vName = varNameTableFieldP table field in
                                            let lName = mkName "_protected_label" in
                                            let lDec = ValD (VarP lName) (NormalB label) [] in
                                            BindS (VarP vName) $ LetE [lDec] $ AppE (AppE (VarE 'toLabeledTCB) (VarE lName)) $ DoE [
                                                    NoBindS $ AppE (VarE 'taintLabel) (VarE lName),
                                                    NoBindS $ AppE (VarE 'return) (VarE $ varNameTableField table field)
                                                ]
                                          else
                                            NoBindS $ AppE (VarE 'taintLabel) label
                                    in
                                    stmt:acc
                                ReqEntity _table _ False ->
                                    acc
                                ReqEntity table optional True ->
                                    let optionCase base handler = AppE (AppE (AppE (VarE 'maybe) (AppE (VarE 'return) base)) handler) (VarE $ varNameTableField table "maybe") in
                                    let nonoptionCase handler = AppE handler $ VarE $ varNameTableE table in
                                    let stmt = if protected then
                                            let vName = varNameTableP table in
                                            BindS (VarP vName) $ if optional then
                                                let tName = mkName "_tmp" in
                                                optionCase (ConE 'Nothing) $ LamE [VarP tName] $ parenInfixE (AppE (VarE 'toProtected) (VarE tName)) (VarE '(>>=)) $ parenInfixE (VarE 'return) (VarE '(.)) (ConE 'Just)
                                              else
                                                nonoptionCase $ VarE 'toProtected
                                          else
                                            let taintEntityLabel = AppE (VarE 'taintLabel) (VarE 'getEntityLabel) in
                                            NoBindS $ if optional then
                                                optionCase (ConE '()) taintEntityLabel
                                              else
                                                nonoptionCase taintEntityLabel
                                    in
                                    stmt:acc
                            ) [] terms 
                      in
                      let returns = NoBindS $ AppE (VarE 'return) $ TupE $ List.foldr (\rterm acc -> case rterm of
                                ReqField table field ret _ -> 
                                    if ret then
                                        let vName = (if protected then varNameTableFieldP else varNameTableField) table field in
                                        (VarE vName):acc
                                    else
                                        acc
                                ReqEntity table optional _ -> 
                                    let name = if protected then
                                            varNameTableP table
                                          else
                                            if optional then
                                              varNameTableField table "maybe"
                                            else
                                              varNameTableE table
                                    in
                                    (VarE name):acc
                            ) [] terms
                      in
                      DoE $ taints ++ [returns]
                in
                LamE [pat] body
          in
          NoBindS $ AppE (VarE 'lift)$ AppE (AppE (VarE 'mapM) fun) (VarE res)

    -- error $ pprint $ DoE [ query, taint]
    return $ DoE [ query, taint]

    where
        mkEntityPattern table = 
            AsP (varNameTableE table) $ ConP 'Entity [ VarP $ varNameTableField table "id", VarP $ varNameTable table]

        mkQueryPatternTables (Table table) = VarP $ varNameTable table
        mkQueryPatternTables (Tables ts j table _) = 
            let constr = case j of
                  InnerJoin -> 'Esq.InnerJoin
                  LeftOuterJoin -> 'Esq.LeftOuterJoin
                  RightOuterJoin -> 'Esq.RightOuterJoin
                  FullOuterJoin -> 'Esq.FullOuterJoin
                  -- CrossJoin -> 'CrossJoin
            in
            ConP constr [ mkQueryPatternTables ts, VarP $ varNameTable table]

        mkWhere _ Nothing = []
        mkWhere isTableOptional (Just (Where expr)) = [NoBindS $ AppE (VarE 'Esq.where_) $ mkExprBExpr isTableOptional expr]

        -- hasLabelsHelper tableS f = List.foldl' (\acc ent -> 
        --     if acc || (lEntityHaskell ent) /= tableS then
        --         acc 
        --     else 
        --         List.foldl' (\acc field ->
        --             if acc || f field then
        --                 acc
        --             else
        --                 maybe False (\_ -> True) $ lFieldLabelAnnotations field
        --           ) False $ lEntityFields ent
        --   ) False lEntityDefs
        -- hasLabelsTable tableS = hasLabelsHelper tableS (\_ -> False)
        -- hasLabelsTableField tableS fieldS = hasLabelsHelper tableS $ \f -> (lFieldHaskell f) /= fieldS

        mkOrderBy _ Nothing = []
        mkOrderBy isTableOptional (Just (OrderBy ords')) = 
            let helper ord = 
                  let ( op,( table, field)) = case ord of
                        OrderAsc t -> ( 'Esq.asc, extractTableField t)
                        OrderDesc t -> ( 'Esq.desc, extractTableField t)
                  in
                  AppE (VarE op) $ mkExprTF (isTableOptional table) table field
                  -- AppE (VarE op) $ mkExprTF False table field
            in
            let ords = List.map helper ords' in
            [NoBindS $ AppE (VarE 'Esq.orderBy) $ ListE ords]

        mkLimit Nothing = []
        mkLimit (Just (Limit limit)) = [NoBindS $ AppE (VarE 'Esq.limit) $ LitE $ IntegerL limit]
        
        mkOffset Nothing = []
        mkOffset (Just (Offset offset)) = [NoBindS $ AppE (VarE 'Esq.offset) $ LitE $ IntegerL offset]

        mkOnTables _ (Table _table) = []
        mkOnTables isTableOptional (Tables ts _ _ bexpr@(BExprBinOp (BTerm _term1) BinEq (BTerm _term2))) = 
            (NoBindS $ AppE (VarE 'Esq.on) $ mkExprBExpr isTableOptional bexpr):(mkOnTables isTableOptional ts)
        mkOnTables _ (Tables _ _ table _) = error $ "mkOnTables: Invalid on expression for table `" ++ table ++ "`"

        mkExprBExpr isTableOptional (BExprBinOp (BTerm term1) op' (BTerm term2)) = 
            let (table1,field1) = extractTableField term1 in
            let (table2,field2) = extractTableField term2 in
            let tableOptional1 = isTableOptional table1 in -- TODO: This is probably incorrect?? Need to consider the relationship between the two tables? XXX
            let tableOptional2 = isTableOptional table2 in
            let expr1' = mkExprTF tableOptional1 table1 field1 in
            let expr2' = mkExprTF tableOptional2 table2 field2 in
            let fieldOptional1 = isTableFieldOptional table1 field1 in
            let fieldOptional2 = isTableFieldOptional table2 field2 in
            let optional1 = tableOptional1 || fieldOptional1 in
            let optional2 = tableOptional2 || fieldOptional2 in
            let ( expr1, expr2) = case ( optional1, optional2) of
                  ( True, False) ->
                    ( expr1', AppE (VarE 'Esq.just) expr2')
                  ( False, True) ->
                    ( AppE (VarE 'Esq.just) expr1', expr2')
                  _ ->
                    ( expr1', expr2')
            in
            let op = mkExprBOp op' in
            --error $ (show tableOptional1) ++":"++ (show tableOptional2) ++":"++ (show optional1) ++":"++ (show optional2)
            parenInfixE expr1 op expr2
        mkExprBExpr isTableOptional (BExprBinOp b1 op b2) = parenInfixE
            (mkExprB isTableOptional b1) (mkExprBOp op) (mkExprB isTableOptional b2)
        mkExprBExpr isTableOptional (BExprAnd e1 e2) = parenInfixE 
            (mkExprBExpr isTableOptional e1) (VarE '(Esq.&&.)) (mkExprBExpr isTableOptional e2)
        mkExprBExpr isTableOptional (BExprOr e1 e2) = parenInfixE 
            (mkExprBExpr isTableOptional e1) (VarE '(Esq.||.)) (mkExprBExpr isTableOptional e2)
        mkExprBExpr isTableOptional (BExprNull t) = AppE (VarE 'Esq.isNothing) $ mkExprTerm isTableOptional t
        mkExprBExpr isTableOptional (BExprNotNull t) = AppE (VarE 'Esq.not_) $ AppE (VarE 'Esq.isNothing) $ mkExprTerm isTableOptional t
        mkExprBExpr isTableOptional (BExprNot expr) = AppE (VarE 'Esq.not_) $ mkExprBExpr isTableOptional expr

        mkExprBOp BinEq = VarE '(Esq.==.)
        mkExprBOp BinNEq = VarE '(Esq.!=.)
        mkExprBOp BinGE = VarE '(Esq.>=.)
        mkExprBOp BinG = VarE '(Esq.>.)
        mkExprBOp BinLE = VarE '(Esq.<=.)
        mkExprBOp BinL = VarE '(Esq.<.)

        mkExprB isTableOptional (BTerm t) = mkExprTerm isTableOptional t
        mkExprB _ (BAnti s) = case Meta.parseExp s of
            Left e -> error e
            Right e -> AppE (VarE 'Esq.val) $ e
        mkExprB _ (BConst c) = mkExprConst c 

        mkExprConst (CBool True) = AppE (VarE 'Esq.val) $ ConE 'True
        mkExprConst (CBool False) = AppE (VarE 'Esq.val) $ ConE 'False
        mkExprConst (CString s) = AppE (VarE 'Esq.val) $ LitE $ StringL s
        mkExprConst (CInt i) = AppE (VarE 'Esq.val) $ LitE $ IntegerL i
        mkExprConst (CDouble d) = AppE (VarE 'Esq.val) $ LitE $ DoublePrimL $ toRational d

        mkExprTF tableOptional table field = 
            let op = VarE $ if tableOptional then '(Esq.?.) else '(Esq.^.) in
            let fieldName = constrNameTableField table field in
            let var = varNameTable table in
            parenInfixE (VarE var) op (ConE fieldName)

        mkExprTerm isTableOptional term = 
            let (tableS, fieldS) = extractTableField term in
            mkExprTF (isTableOptional tableS) tableS fieldS

        getLTable tableS = 
            let findEntity [] = error $ "Could not find table `" ++ tableS ++ "`"
                findEntity (h:t) = 
                    if toLowerString (lEntityHaskell h) == toLowerString tableS then
                        h
                    else
                        findEntity t
            in
            findEntity lEntityDefs

        getLTableField tableS fieldS = 
            if fieldS == "id" then
                let typ = FTTypeCon Nothing (Text.pack $ tableS ++ "Id") in
                LFieldDef fieldS typ True ([],[])
            else
                let ent = getLTable tableS in
                -- let findField [] = error $ "Could not find field `" ++ fieldS ++ "` for table `" ++ tableS ++ "`"
                --     findField (h:t) = 
                --         if toLowerString (lFieldHaskell h) == toLowerString fieldS then
                --             h
                --         else
                --             findField t
                -- in
                -- findField $ lEntityFields ent
                getLEntityFieldDef ent fieldS

        isTableFieldOptional tableS fieldS =
            case lFieldType $ getLTableField tableS fieldS of
                FTApp (FTTypeCon _ "Maybe") _ ->
                    True
                _ ->
                    False


        extractTableField (TermTF t (Field f)) = ( t, f)
        extractTableField (TermTF t FieldAll) = error $ "extractTableField: All fields requested for table `" ++ t ++ "`"
        extractTableField (TermF f) = error $ "extractTableField: Invalid terminal field `TermF " ++ (show f) ++ "`"

        toLowerString = List.map Char.toLower
        varNameTable table = mkName $ '_':(toLowerString table)
        varNameTableE table = mkName $ '_':'e':'_':(toLowerString table)
        varNameTableP table = mkName $ '_':'p':'_':(toLowerString table)
        varNameTableField table field = mkName $ '_':((toLowerString table) ++ ('_':(toLowerString field)))
        varNameTableFieldP table field = mkName $ '_':'p':'_':((toLowerString table) ++ ('_':(toLowerString field)))
        constrNameTableField table field = mkName $ (headToUpper table) ++ (headToUpper field)

        reqTermsCommand isTableOptional (Command _ terms tables whereM orderByM _limitM _offsetM) = 
            -- Get all requested terms
            --    transform to ReqTerm
            -- get the dependencies of all the other terms
            --    union (and transform) into rest of dependency terms, requested false
            let terms' = case terms of 
                  Terms terms -> terms
                  TermsAll -> error "reqTermsCommand: normalization failed"
            in
            let reqTerms = List.map (reqTermsTerm isTableOptional True) terms' in
            let reqTerms' = reqTermsTables isTableOptional reqTerms tables in
            let reqTerms'' = maybe reqTerms' (reqTermsWhere isTableOptional reqTerms') whereM in
            maybe reqTerms'' (reqTermsOrderBy isTableOptional reqTerms'') orderByM
            --let reqTerms''' = maybe reqTerms'' (reqTermsOrderBy reqTerms'') orderByM in
            --let reqTerms'''' = maybe reqTerms''' (reqTermsLimit reqTerms''') limitM in
            --maybe reqTerms'''' (reqTermsOffset reqTerms'''') offsetM

        reqTermsOrderBy isTableOptional curTerms (OrderBy ords) = List.foldl' (\acc ord -> case ord of
                OrderAsc t ->
                    reqTermsTermMaybe isTableOptional acc t
                OrderDesc t ->
                    reqTermsTermMaybe isTableOptional acc t
            ) curTerms ords

        reqTermsWhere isTableOptional curTerms (Where bexpr) = reqTermsBExpr isTableOptional curTerms bexpr

        reqTermsTables :: (String -> Bool) -> [ReqTerm] -> Tables -> [ReqTerm]
        reqTermsTables isTableOptional curTerms (Tables ts _ _ bexpr) = 
            reqTermsTables isTableOptional (reqTermsBExpr isTableOptional curTerms bexpr) ts
        reqTermsTables _ curTerms (Table _) = curTerms

        reqTermsBExpr :: (String -> Bool) -> [ReqTerm] -> BExpr -> [ReqTerm]
        reqTermsBExpr isTableOptional curTerms (BExprAnd e1 e2) = reqTermsBExpr isTableOptional (reqTermsBExpr isTableOptional curTerms e1) e2
        reqTermsBExpr isTableOptional curTerms (BExprOr e1 e2) = reqTermsBExpr isTableOptional (reqTermsBExpr isTableOptional curTerms e1) e2
        reqTermsBExpr isTableOptional curTerms (BExprBinOp b1 _ b2) = reqTermsB isTableOptional (reqTermsB isTableOptional curTerms b1) b2
        reqTermsBExpr isTableOptional curTerms (BExprNull t) = reqTermsTermMaybe isTableOptional curTerms t
        reqTermsBExpr isTableOptional curTerms (BExprNotNull t) = reqTermsTermMaybe isTableOptional curTerms t
        reqTermsBExpr isTableOptional curTerms (BExprNot e) = reqTermsBExpr isTableOptional curTerms e

        reqTermsB :: (String -> Bool) -> [ReqTerm] -> B -> [ReqTerm]
        reqTermsB isTableOptional curTerms (BTerm t) = reqTermsTermMaybe isTableOptional curTerms t
        reqTermsB _ curTerms _ = curTerms

        -- Union in new term. Term should never be an entity.
        reqTermsTermMaybe :: (String -> Bool) -> [ReqTerm] -> Term -> [ReqTerm]
        reqTermsTermMaybe isTableOptional curTerms term = 
            let ( tableS, fieldS) = extractTableField term in
            let reqTerm = reqTermsTerm isTableOptional False term in
            let cons = List.foldl' (\acc term -> case term of
                    ReqEntity tableS' _ _ ->
                        if tableS == tableS' then
                            False
                        else
                            acc
                    ReqField tableS' fieldS' _returning _ ->
                        if tableS == tableS' && fieldS == fieldS' then
                            False
                        else
                            acc
                  ) True curTerms
            in
            if cons then
                reqTerm:curTerms
            else
                curTerms

        reqTermsTerm isTableOptional returning (TermTF tableS field) = case field of
            Field fieldS ->
                let fieldDef = getLTableField tableS fieldS in
                let dep = if readLabelIsBottom $ lFieldLabelAnnotations fieldDef then
                        Nothing
                      else
                        let ( anns, _) = lFieldLabelAnnotations fieldDef in
                        Just $ List.foldl' (\acc ann -> case ann of
                            LAId ->
                                ( tableS, "id"):acc
                            LAConst _s ->
                                acc
                            LAField f -> 
                                ( tableS, f):acc
                          ) [] anns
                in
                ReqField tableS fieldS returning dep
            FieldAll ->
                let hasDeps = 
                      let ent = getLTable tableS in
                      List.foldl' (\acc f -> acc || (not $ readLabelIsBottom $ lFieldLabelAnnotations f)) False $ lEntityFields ent
                in
                let optional = isTableOptional tableS in
                -- if (lEntityHaskell $ getLTable tableS) == "User" then
                --     error $ show $ getLTable tableS
                -- else
                ReqEntity tableS optional hasDeps
        reqTermsTerm _ _ (TermF _) = error "reqTermsTerm: normalization failed"

        -- selectE = VarE $ mkName "select"
        -- fromE = VarE $ mkName "from"
        -- where_E = VarE $ mkName "where_"
        -- onE = VarE $ mkName "on"
        -- justE = VarE $ mkName "just"
        -- carotE = VarE $ mkName "^."
        -- questionE = VarE $ mkName "?."
        -- eqE = VarE $ mkName "==."
        -- geE = VarE $ mkName ">=."
        -- gE = VarE $ mkName ">."
        -- leE = VarE $ mkName "<=."
        -- lE = VarE $ mkName "<."
        -- innerJoin = VarE $ mkName "InnerJoin"
        -- leftOuterJoin = VarE $ mkName "LeftOuterJoin"
        -- rightOuterJoin = VarE $ mkName "RightOuterJoin"
        -- fullOuterJoin = VarE $ mkName "FullOuterJoin"
        -- crossJoin = VarE $ mkName "CrossJoin"

data ReqTerm = 
    ReqField {
        _reqFieldTable :: String
      , _reqFieldField :: String
--      , _reqFieldIsOptional :: Bool
      , _reqFieldReturning :: Bool
      -- JP: Drop this optional? XXX
      , _reqFieldDependencies :: Maybe [(String,String)] -- Contains ( table, field) dependencies.
--      , _reqFieldIsDependency :: Bool
    }
  | ReqEntity {
        _reqEntityTable :: String -- Implied returning is true
      , _reqEntityIsOptional :: Bool
      , _reqEntityHasLabels :: Bool
    }

    deriving (Show)

-- | Normalize an AST by adding table name for all terms. Also expands out all the tables requested when TermsAll is applied. 
normalizeTerms :: Command -> Command
normalizeTerms (Command select terms tables whereM orderByM limitM offsetM) = 
    -- Get the default table if there are no joins. 
    let defTable = case tables of 
          Table table ->
            Just table
          _ ->
            Nothing
    in
    let terms' = case terms of
          TermsAll ->
            -- Expand all the tables out. 
            let expander acc tables = case tables of
                  Table table ->
                    (TermTF table FieldAll):acc
                  Tables tables _ table _ ->
                    expander ((TermTF table FieldAll):acc) tables
            in
            Terms $ expander [] tables
          Terms terms' -> 
            -- Add table name to each term. 
            Terms $ List.map (updateTerm defTable) terms'
    in
    -- This checks that terms should already have tables included. 
    let tables' = updateTables defTable tables in
    let whereM' = case whereM of 
          Just (Where bexpr) -> 
            Just $ Where $ updateBExpr defTable bexpr
          Nothing -> 
            Nothing
    in
    let orderByM' = case orderByM of
          Just (OrderBy orders) -> 
            let helper ord = case ord of
                  OrderAsc t ->
                    OrderAsc $ updateTerm defTable t
                  OrderDesc t ->
                    OrderDesc $ updateTerm defTable t
            in
            Just $ OrderBy $ List.map helper orders
          Nothing -> 
            Nothing
    in
    Command select terms' tables' whereM' orderByM' limitM offsetM

    where
        updateTerm defTable term = case term of
            TermF field ->
                maybe 
                    (error $ "Could not infer table associated with field `" ++ (show field) ++ "`")
                    (\table -> TermTF table field)
                    defTable
            _ ->
                term
        
        updateTables defTable (Tables ts j table bexpr) =
            Tables (updateTables defTable ts) j table $ updateBExpr defTable bexpr
        updateTables _ t = t

        updateBExpr defTable bexpr = case bexpr of
            BExprAnd expr1 expr2 ->
                BExprAnd (updateBExpr defTable expr1) (updateBExpr defTable expr2)
            BExprOr expr1 expr2 ->
                BExprOr (updateBExpr defTable expr1) (updateBExpr defTable expr2)
            BExprBinOp b1 op b2 ->
                BExprBinOp (updateB defTable b1) op (updateB defTable b2)
            BExprNull t ->
                BExprNull $ updateTerm defTable t
            BExprNotNull t ->
                BExprNotNull $ updateTerm defTable t
            BExprNot expr ->
                BExprNot $ updateBExpr defTable expr

        updateB defTable (BTerm t) = BTerm $ updateTerm defTable t
        updateB _ b = b

parenInfixE :: Exp -> Exp -> Exp -> Exp
parenInfixE e1 e2 e3 = ParensE $ UInfixE e1 e2 e3

-- Debugging functions.

lsqlHelper' :: [LEntityDef] -> QuasiQuoter
lsqlHelper' ents = QuasiQuoter {
        quoteExp = (generateSql' ents) . Text.pack
    }
    where 
        generateSql' e s = do
            res <- generateSql e s
            error $ pprint res
