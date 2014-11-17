{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Database.LEsqueleto (mkLSql, module Export) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Database.Esqueleto as Esq
import Database.Esqueleto as Export (Value(..))
import Data.Maybe (isJust)
import Database.Persist.Types
import Data.Text (Text)
import qualified Data.Text as Text
import Database.LPersist
--import Database.Persist
import qualified Language.Haskell.Meta.Parse as Meta
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import LMonad

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
            let fields = mkSerializedLFieldsDef $ lEntityFields ent in
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
                  let anns = maybe (ConE 'Nothing) (\( r', w', c') -> 
                            let r = ListE $ map mkSerializedLabelAnnotation r' in
                            let w = ListE $ map mkSerializedLabelAnnotation w' in
                            let c = ListE $ map mkSerializedLabelAnnotation c' in
                            AppE (ConE 'Just) $ TupE [ r, w, c]
                        ) $ lFieldLabelAnnotations field 
                  in
                  AppE (AppE (AppE (AppE (ConE 'LFieldDef) name) typ) strict) anns
            in
            ListE $ map helper fields'

lsqlHelper :: [LEntityDef] -> QuasiQuoter
lsqlHelper ents = QuasiQuoter {
        quoteExp = (generateSql ents) . Text.pack
    }

--parse :: (Label l, PersistConfig c, LMonad m, m ~ HandlerT site IO) => Text -> PersistConfigBackend c (LMonadT l m) b
--generateSql :: (Label l, m ~ HandlerT site IO, YesodLPersist site) => ReaderT (YesodPersistBackend site) (LMonadT l m) a 
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
    let terms = reqTermsCommand normalized in 
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
                ( [( table, lvl)], lvl)
          in
          let ( mapping, _) = createAssoc (commandTables normalized) (0 :: Int) in
          maybe 
            (error $ "Could not find table `" ++ tableS ++ "`") 
            (> 0) 
            $ List.lookup tableS mapping
    in
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
                        mkExprTF False table field -- TODO: Does the option matter here??? XXX
                    ReqEntity ent _ -> 
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
                                VarP $ varNameTableField table field
                              ReqEntity table _ ->
                                AsP (varNameTableE table) $ ConP 'Entity [ VarP $ varNameTableField table "id", VarP $ varNameTable table]
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
                                    ReqEntity table' _
                                      | table == table' ->
                                        if field == "id" then
                                            Just $ VarE $ varNameTableField table field
                                        else
                                            let getter = mkName $ (headToLower table) ++ (headToUpper field) in
                                            Just $ AppE (VarE getter) $ VarE $ varNameTable table
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
                                    let tainter = VarE $ mkName $ "read" ++ (headToUpper table) ++ (headToUpper field) ++ "Label'" in
                                    let taint = AppE (VarE 'taintLabel) $ List.foldl' (\acc (table',field') -> 
                                            AppE acc $ getExpr table' field'
                                          ) tainter deps
                                    in
                                    (NoBindS taint):acc
                                ReqEntity table False ->
                                    acc
                                ReqEntity table True ->
                                    (NoBindS $ AppE (VarE 'raiseLabelRead) (VarE $ varNameTable table)):acc
                                
                            ) [] terms in
                      let returns = NoBindS $ AppE (VarE 'return) $ TupE $ List.foldr (\rterm acc -> case rterm of
                                ReqField table field ret _ -> 
                                    if ret then
                                        (VarE $ varNameTableField table field):acc
                                    else
                                        acc
                                ReqEntity table _ -> 
                                    (VarE $ varNameTableE table):acc
                            ) [] terms in

                      DoE $ taints ++ [returns]
                in
                LamE [pat] body
          in
          NoBindS $ AppE (AppE (VarE 'mapM) fun) (VarE res)

    -- error $ pprint $ DoE [ query, taint]
    return $ DoE [ query, taint]

    where
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

        hasLabelsHelper tableS f = List.foldl' (\acc ent -> 
            if acc || (lEntityHaskell ent) /= tableS then
                acc 
            else 
                List.foldl' (\acc field ->
                    if acc || f field then
                        acc
                    else
                        maybe False (\_ -> True) $ lFieldLabelAnnotations field
                  ) False $ lEntityFields ent
          ) False lEntityDefs
        hasLabelsTable tableS = hasLabelsHelper tableS (\_ -> False)
        hasLabelsTableField tableS fieldS = hasLabelsHelper tableS $ \f -> (lFieldHaskell f) /= fieldS

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

        mkOnTables _ (Table table) = []
        mkOnTables isTableOptional (Tables ts _ _ bexpr@(BExprBinOp (BTerm term1) BinEq (BTerm term2))) = 
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
            let optional2 = fieldOptional1 || fieldOptional2 in
            let ( expr1, expr2) = case ( optional1, optional2) of
                  ( True, False) ->
                    ( expr1', AppE (VarE 'Esq.just) expr2')
                  ( False, True) ->
                    ( AppE (VarE 'Esq.just) expr1', expr2')
                  _ ->
                    ( expr1', expr2')
            in
            let op = mkExprBOp op' in
            UInfixE expr1 op expr2
        mkExprBExpr _ (BExprBinOp b1 op b2) = UInfixE
            (mkExprB b1) (mkExprBOp op) (mkExprB b2)
        mkExprBExpr isTableOptional (BExprAnd e1 e2) = UInfixE 
            (mkExprBExpr isTableOptional e1) (VarE '(Esq.&&.)) (mkExprBExpr isTableOptional e2)
        mkExprBExpr isTableOptional (BExprOr e1 e2) = UInfixE 
            (mkExprBExpr isTableOptional e1) (VarE '(Esq.||.)) (mkExprBExpr isTableOptional e2)
        mkExprBExpr isTableOptional (BExprNull t) = AppE (VarE 'Esq.isNothing) $ mkExprTerm False t
        mkExprBExpr isTableOptional (BExprNotNull t) = AppE (VarE 'Esq.not_) $ AppE (VarE 'Esq.isNothing) $ mkExprTerm False t
        mkExprBExpr isTableOptional (BExprNot expr) = AppE (VarE 'Esq.not_) $ mkExprBExpr isTableOptional expr

        mkExprBOp BinEq = VarE '(Esq.==.)
        mkExprBOp BinNEq = VarE '(Esq.!=.)
        mkExprBOp BinGE = VarE '(Esq.>=.)
        mkExprBOp BinG = VarE '(Esq.>.)
        mkExprBOp BinLE = VarE '(Esq.<=.)
        mkExprBOp BinL = VarE '(Esq.<.)

        mkExprB (BTerm t) = mkExprTerm False t
        mkExprB (BAnti s) = case Meta.parseExp s of
            Left e -> error e
            Right e -> AppE (VarE 'Esq.val) $ e
        mkExprB (BConst c) = mkExprConst c 

        mkExprConst (CBool True) = AppE (VarE 'Esq.val) $ ConE 'True
        mkExprConst (CBool False) = AppE (VarE 'Esq.val) $ ConE 'False
        mkExprConst (CString s) = AppE (VarE 'Esq.val) $ LitE $ StringL s
        mkExprConst (CInt i) = AppE (VarE 'Esq.val) $ LitE $ IntegerL i
        mkExprConst (CDouble d) = AppE (VarE 'Esq.val) $ LitE $ DoublePrimL $ toRational d

        mkExprTF tableOptional table field = 
            let op = VarE $ if tableOptional then '(Esq.?.) else '(Esq.^.) in
            let fieldName = constrNameTableField table field in
            let var = varNameTable table in
            UInfixE (VarE var) op (ConE fieldName)

        mkExprTerm optional term = 
            let (tableS, fieldS) = extractTableField term in
            mkExprTF optional tableS fieldS

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
                LFieldDef fieldS typ True Nothing
            else
                let ent = getLTable tableS in
                let findField [] = error $ "Could not find field `" ++ fieldS ++ "` for table `" ++ tableS ++ "`"
                    findField (h:t) = 
                        if toLowerString (lFieldHaskell h) == toLowerString fieldS then
                            h
                        else
                            findField t
                in
                findField $ lEntityFields ent

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
        varNameTableField table field = mkName $ '_':((toLowerString table) ++ ('_':(toLowerString field)))
        constrNameTableField table field = mkName $ (headToUpper table) ++ (headToUpper field)

        reqTermsCommand (Command _ terms tables whereM orderByM _limitM _offsetM) = 
            -- Get all requested terms
            --    transform to ReqTerm
            -- get the dependencies of all the other terms
            --    union (and transform) into rest of dependency terms, requested false
            let terms' = case terms of 
                  Terms terms -> terms
                  TermsAll -> error "reqTermsCommand: normalization failed"
            in
            let reqTerms = List.map (reqTermsTerm True) terms' in
            let reqTerms' = reqTermsTables reqTerms tables in
            let reqTerms'' = maybe reqTerms' (reqTermsWhere reqTerms') whereM in
            maybe reqTerms'' (reqTermsOrderBy reqTerms'') orderByM
            --let reqTerms''' = maybe reqTerms'' (reqTermsOrderBy reqTerms'') orderByM in
            --let reqTerms'''' = maybe reqTerms''' (reqTermsLimit reqTerms''') limitM in
            --maybe reqTerms'''' (reqTermsOffset reqTerms'''') offsetM

        reqTermsOrderBy curTerms (OrderBy ords) = List.foldl' (\acc ord -> case ord of
                OrderAsc t ->
                    reqTermsTermMaybe acc t
                OrderDesc t ->
                    reqTermsTermMaybe acc t
            ) curTerms ords

        reqTermsWhere curTerms (Where bexpr) = reqTermsBExpr curTerms bexpr

        reqTermsTables curTerms (Tables ts _ _ bexpr) = 
            reqTermsTables (reqTermsBExpr curTerms bexpr) ts
        reqTermsTables curTerms (Table _) = curTerms

        reqTermsBExpr curTerms (BExprAnd e1 e2) = reqTermsBExpr (reqTermsBExpr curTerms e1) e2
        reqTermsBExpr curTerms (BExprOr e1 e2) = reqTermsBExpr (reqTermsBExpr curTerms e1) e2
        reqTermsBExpr curTerms (BExprBinOp b1 _ b2) = reqTermsB (reqTermsB curTerms b1) b2
        reqTermsBExpr curTerms (BExprNull t) = reqTermsTermMaybe curTerms t
        reqTermsBExpr curTerms (BExprNotNull t) = reqTermsTermMaybe curTerms t
        reqTermsBExpr curTerms (BExprNot e) = reqTermsBExpr curTerms e

        reqTermsB curTerms (BTerm t) = reqTermsTermMaybe curTerms t
        reqTermsB curTerms _ = curTerms

        -- Union in new term. Term should never be an entity.
        reqTermsTermMaybe :: [ReqTerm] -> Term -> [ReqTerm]
        reqTermsTermMaybe curTerms term = 
            let ( tableS, fieldS) = extractTableField term in
            let reqTerm = reqTermsTerm False term in
            let cons = List.foldl' (\acc term -> case term of
                    ReqEntity tableS' _ ->
                        if tableS == tableS' then
                            False
                        else
                            acc
                    ReqField tableS' fieldS' returning _ ->
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

        reqTermsTerm returning (TermTF tableS field) = case field of
            Field fieldS ->
                let fieldDef = getLTableField tableS fieldS in
                let dep = maybe Nothing (\( anns, _, _) -> Just $ List.foldl' (\acc ann -> case ann of
                        LAId ->
                            ( tableS, "id"):acc
                        LAConst s ->
                            acc
                        LAField f -> 
                            ( tableS, f):acc
                      ) [] anns ) $ lFieldLabelAnnotations fieldDef in
                ReqField tableS fieldS returning dep
            FieldAll ->
                let hasDeps = 
                      let ent = getLTable tableS in
                      List.foldl' (\acc f -> acc || isJust (lFieldLabelAnnotations f)) False $ lEntityFields ent
                in
                ReqEntity tableS hasDeps
        reqTermsTerm _ (TermF _) = error "reqTermsTerm: normalization failed"

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
        reqFieldTable :: String
      , reqFieldField :: String
--      , reqFieldIsMaybe :: Bool
      , reqFieldReturning :: Bool
      , reqFieldDependencies :: Maybe [(String,String)] -- Contains ( table, field) dependencies.
--      , reqFieldIsDependency :: Bool
    }
  | ReqEntity {
        reqEntityTable :: String -- Implied returning is true
      , hasLabels :: Bool
    }

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



-- | Represent the AST for lsql statements. 
data Command = Command {
        commandSelect :: Select
      , commandTerms :: Terms
      , commandTables :: Tables
      , commandWhere :: Maybe Where
      , commandOrderBy :: Maybe OrderBy
      , commandLimit :: Maybe Limit
      , commandOffset :: Maybe Offset
    }
    deriving (Show)
data Select = Select | PSelect
    deriving (Show)

--data Terms = Term Term | Terms Terms Term | TermsAll
data Terms = Terms [Term] | TermsAll
    deriving (Show)

data Tables = Table String | Tables Tables Join String BExpr
    deriving (Show)
data Join = InnerJoin | LeftOuterJoin | RightOuterJoin | FullOuterJoin -- | CrossJoin
    deriving (Show)

data Where = Where BExpr
    deriving (Show)

data OrderBy = OrderBy [Order]
    deriving (Show)

data Order = OrderAsc Term | OrderDesc Term
    deriving (Show)

data Limit = Limit Integer
    deriving (Show)

data Offset = Offset Integer
    deriving (Show)

data Term = TermTF String TermField | TermF TermField
    deriving (Show)
data TermField = Field String | FieldAll

data BExpr = BExprAnd BExpr BExpr | BExprOr BExpr BExpr | BExprBinOp B BinOp B | BExprNull Term | BExprNotNull Term | BExprNot BExpr
    deriving (Show)

data BinOp = BinEq | BinNEq | BinGE | BinG | BinLE | BinL
    deriving (Show)

data B = BTerm Term | BAnti String | BConst C
    deriving (Show)
data C = CBool Bool | CString String | CInt Integer | CDouble Double
    deriving (Show)

instance Show TermField where
    show (Field s) = s
    show (FieldAll) = "*"

parseCommand :: Parser Command
parseCommand = do
    select <- parseSelect
    terms <- parseTerms
    _ <- parseFrom
    tables <- parseTables
    whereM <- parseWhere
    orderByM <- parseOrderBy
    limitM <- parseLimit
    offsetM <- parseOffset
    return $ Command select terms tables whereM orderByM limitM offsetM

    where
        takeNonSpace = takeWhile1 (not . Char.isSpace)

        takeAlphaNum = takeWhile1 Char.isAlphaNum
        takeUpperAlphaNum = do
            an <- takeAlphaNum
            return $ Text.map Char.toUpper an

        parseSelect = do
            skipSpace
            select <- takeUpperAlphaNum
            case select of
                "SELECT" ->
                    return Select
                "PSELECT" ->
                    return PSelect
                _ ->
                    fail $ "Unknown keywork `" ++ (Text.unpack select) ++ "`. Use `SELECT` or `PSELECT`."

        parseTerms = 
            let parseTerms' = do
                  head <- parseTerm
                  tail <- (do
                        skipSpace
                        _ <- char ','
                        parseTerms'
                    ) <|> (return [])
                  return $ head:tail
            in
            (skipSpace >> char '*' >> (return TermsAll)) <|> (parseTerms' >>= return . Terms)
            
        parseFrom = do
            skipSpace
            asciiCI "FROM"

        parseTerm = do
            skipSpace
            ( do
                table <- takeAlphaNum
                _ <- char '.'
                field <- parseField
                return $ TermTF (Text.unpack table) field
              ) <|> (parseField >>= (return . TermF))

        -- Does not skip spaces!!
        parseField = (char '*' >> (return FieldAll)) <|> 
            ( takeAlphaNum >>= (return . Field. Text.unpack))

        parseTables = 
            let parseTables' acc = ( do
                    skipSpace
                    join <- ( asciiCI "INNER JOIN" >> (return InnerJoin)) <|> 
                      ( asciiCI "OUTER JOIN" >> (return LeftOuterJoin)) <|>
                      ( asciiCI "LEFT OUTER JOIN" >> (return LeftOuterJoin)) <|>
                      ( asciiCI "RIGHT OUTER JOIN" >> (return RightOuterJoin)) <|>
                      ( asciiCI "FULL OUTER JOIN" >> (return FullOuterJoin)) -- <|>
                      --( asciiCI "CROSS JOIN" >> (return CrossJoin))
                    skipSpace
                    table <- takeAlphaNum
                    skipSpace
                    _ <- asciiCI "ON"
                    bexpr <- parseBExpr
                    parseTables' $ Tables acc join (Text.unpack table) bexpr
                  ) <|> (return acc)
            in
            do
            skipSpace
            table <- takeAlphaNum
            parseTables' $ Table $ Text.unpack table

        parseWhere = ( do
            skipSpace
            _ <- asciiCI "WHERE"
            bexp <- parseBExpr
            return $ Just $ Where bexp
          ) <|> (return Nothing)
        
        parseOrderBy = ( do
            skipSpace
            _ <- asciiCI "ORDER BY"
            orders <- parseOrders
            return $ Just $ OrderBy orders
          ) <|> (return Nothing)
            
            where
                parseOrders = do
                    term <- parseTerm
                    order <- ( skipSpace >> asciiCI "ASC" >> (return OrderAsc)) <|> 
                        ( skipSpace >> asciiCI "DESC" >> (return OrderDesc)) <|> 
                        ( return OrderAsc)
                    tail <- ( do
                        skipSpace
                        _ <- asciiCI ","
                        parseOrders
                      )
                    return $ (order term):tail

        parseLimit = ( do
            skipSpace
            _ <- asciiCI "LIMIT"
            skipSpace
            limit <- decimal
            return $ Just $ Limit limit
          ) <|> (return Nothing)
        
        parseOffset = ( do
            skipSpace
            _ <- asciiCI "OFFSET"
            skipSpace
            limit <- decimal
            return $ Just $ Offset limit
          ) <|> (return Nothing)

        parseBExpr = do
            expr1 <- ( do
                skipSpace
                _ <- char '('
                res <- parseBExpr
                skipSpace
                _ <- char ')'
                return res
              ) <|> ( do
                skipSpace
                _ <- asciiCI "NOT"
                res <- parseBExpr
                return $ BExprNot res
              ) <|> ( do
                term <- parseTerm
                skipSpace
                _ <- asciiCI "IS NULL"
                return $ BExprNull term
              ) <|> ( do
                term <- parseTerm
                skipSpace
                _ <- asciiCI "IS NOT NULL"
                return $ BExprNotNull term
              ) <|> ( do
                b1 <- parseB
                op <- parseBOp
                b2 <- parseB
                return $ BExprBinOp b1 op b2
              )
            ( do
                skipSpace
                -- temp <- takeAlphaNum
                -- when (temp /= "where" && temp /= "and") $ 
                --     error $ "here: " ++ (show expr1) ++ " **** " ++ (Text.unpack temp)
                constr <- (asciiCI "AND" >> (return BExprAnd)) <|>
                    (asciiCI "OR" >> peekChar >>= (maybe (return BExprOr) $ \c -> 
                        if c /= ' ' then
                            fail "OR: Some other keyword"
                        else
                            return BExprOr
                      ))
                expr2 <- parseBExpr
                return $ constr expr1 expr2
              ) <|> (return expr1)

          where
            parseSQLString = takeWhile1 (/= '\'')

            parseConst = skipSpace >> 
                ( asciiCI "TRUE" >> return (CBool True)) <|>
                ( asciiCI "FALSE" >> return (CBool False)) <|>
                ( do
                    _ <- char '\'' 
                    skipSpace
                    str <- parseSQLString
                    skipSpace
                    _ <- char '\'' 
                    return $ CString $ Text.unpack str
                ) <|> ( do
                    int <- signed decimal
                    next <- peekChar
                    case next of 
                        Just '.' ->
                            fail "this is a double"
                        _ ->
                            return $ CInt int
                ) <|>
                ( double >>= (return . CDouble))

            parseB = ( do
                skipSpace
                _ <- asciiCI "#{"
                skipSpace
                var <- takeWhile1 (/= '}')--takeNonSpace -- TODO: maybe make this into a [String] and stop at '}'
                skipSpace
                _ <- char '}'
                return $ BAnti $ Text.unpack var
              ) <|> ( do
                term <- parseTerm
                return $ BTerm term
              ) <|> ( do
                c <- parseConst
                return $ BConst c
              )

            parseBOp = do
                skipSpace
                op <- takeNonSpace
                return $ case op of
                    "==" -> BinEq
                    "!=" -> BinNEq
                    ">=" -> BinGE
                    ">" -> BinG
                    "<=" -> BinLE
                    "<" -> BinL
                    t -> 
                        error $ "Invalid binop `" ++ (Text.unpack t) ++ "`"
                -- >> ( asciiCI "==" >> return BinEq) <|> 
                -- ( asciiCI ">=" >> return BinGE) <|>
                -- ( char '>' >> return BinG) <|>
                -- ( asciiCI "<=" >> return BinLE) <|>
                -- ( char '<' >> return BinL) <|>
                -- ( takeAlphaNum >>= \t -> fail $ "Invalid binop `" ++ (Text.unpack t) ++ "`")

-- Debugging functions.

lsqlHelper' :: [LEntityDef] -> QuasiQuoter
lsqlHelper' ents = QuasiQuoter {
        quoteExp = (generateSql' ents) . Text.pack
    }
    where 
        generateSql' e s = do
            res <- generateSql e s
            error $ pprint res
