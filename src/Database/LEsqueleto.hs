{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Database.LEsqueleto (lsql) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Reader
import Data.Attoparsec.Text
import qualified Data.Char as Char
import Data.Int (Int64)
import qualified Data.List as List
import Database.Persist.Types
import qualified Data.Text as Text
import Database.LPersist
--import Database.Persist
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import LMonad
import Yesod.Persist.Core

import Internal

lsql :: QuasiQuoter
lsql = QuasiQuoter {
        quoteExp = generateSql . Text.pack
    }

--parse :: (Label l, PersistConfig c, LMonad m, m ~ HandlerT site IO) => Text -> PersistConfigBackend c (LMonadT l m) b
--generateSql :: (Label l, m ~ HandlerT site IO, YesodLPersist site) => ReaderT (YesodPersistBackend site) (LMonadT l m) a 
generateSql s = 
    -- Parse the DSL. 
    let ast = case parseOnly parseCommand s of
          Left err ->
            error $ "Error parsing lsql statement: " ++ err
          Right res ->
            res
    in
    let normalized = normalizeTerms ast in
    let terms = undefined :: [ReqTerm] in
    let isTableOptional = undefined :: String -> Bool in
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
    res <- newName "res"
    let query = 
          let tables = commandTables normalized in
          BindS (VarP res) $ AppE selectE $ AppE fromE $ 
            LamE [mkQueryPatternTables tables] $ DoE 
                ((mkOnTables isTableOptional tables) ++ [])

    let taint = undefined

    return $ DoE [ query, taint]

    where
        mkQueryPatternTables (Table table) = VarP $ varNameTable table
        mkQueryPatternTables (Tables ts j table _) = 
            let constr = case j of
                  InnerJoin -> 'InnerJoin
                  LeftOuterJoin -> 'LeftOuterJoin
                  RightOuterJoin -> 'RightOuterJoin
                  FullOuterJoin -> 'FullOuterJoin
                  -- CrossJoin -> 'CrossJoin
            in
            ConP constr [ mkQueryPatternTables ts, VarP $ varNameTable table]

        mkOnTables _ (Table table) = []
        mkOnTables isTableOptional (Tables ts _ _ bexpr@(BExprBinOp (BTerm term1) BinEq (BTerm term2))) = 
            (mkExprBExpr isTableOptional bexpr):(mkOnTables isTableOptional ts)
        mkOnTables _ (Tables _ _ table _) = error $ "mkOnTables: Invalid on expression for table `" ++ table ++ "`"

        mkExprBExpr isTableOptional (BExprBinOp (BTerm term1) op' (BTerm term2)) = 
            let (table1,field1) = extractTableField term1 in
            let (table2,field2) = extractTableField term2 in
            let tableOptional1 = isTableOptional table1 in -- TODO: This is probably incorrect?? Need to consider the relationship between the two tables? XXX
            let tableOptional2 = isTableOptional table2 in
            let expr1' = mkExprB tableOptional1 table1 field1 in
            let expr2' = mkExprB tableOptional2 table2 field2 in
            let fieldOptional1 = isTableFieldOptional table1 field1 in
            let fieldOptional2 = isTableFieldOptional table2 field2 in
            let optional1 = tableOptional1 || fieldOptional1 in
            let optional2 = fieldOptional1 || fieldOptional2 in
            let ( expr1, expr2) = case ( optional1, optional2) of
                  ( True, False) ->
                    ( expr1', AppE justE expr2')
                  ( False, True) ->
                    ( AppE justE expr1', expr2')
                  _ ->
                    ( expr1', expr2')
            in
            let op = case op' of
                  BinEq -> eqE
                  BinGE -> geE
                  BinG -> gE
                  BinLE -> leE
                  binL -> lE
            in
            NoBindS $ UInfixE expr1 op expr2
        mkExprBExpr _ (BExprBinOp b1 op b2) = error "TODO"
        mkExprBExpr _ _ = error "TODO"

        mkExprB tableOptional table field = 
            let op = if tableOptional then questionE else carotE in
            let fieldName = mkName $ (headToUpper $ toLowerString table) ++ (headToUpper $ toLowerString field) in
            let var = varNameTableField table field in
            UInfixE (VarE var) op (VarE fieldName)


        isTableFieldOptional tableS fieldS =
            let findEntity [] = error $ "Could not find table `" ++ tableS ++ "`"
                findEntity (h:t) = 
                    if toLowerString (lEntityHaskell h) == toLowerString tableS then
                        h
                    else
                        findEntity t
            in
            let ent = findEntity lEntityDefs in -- TODO: Use a typeclass to grab this?? XXX
            let findField [] = error $ "Could not find field `" ++ fieldS ++ "`"
                findField (h:t) = 
                    if toLowerString (lFieldHaskell h) == toLowerString fieldS then
                        h
                    else
                        findField t
            in
            let field = findField $ lEntityFields ent in
            case lFieldType field of
                FTApp (FTTypeCon _ "Maybe") _ ->
                    True
                _ ->
                    False


        extractTableField (TermTF t (Field f)) = ( t, f)
        extractTableField (TermTF t FieldAll) = error $ "extractTableField: All fields requested for table `" ++ t ++ "`"
        extractTableField (TermF f) = error $ "extractTableField: Invalid terminal field `TermF " ++ (show f) ++ "`"

        toLowerString = List.map Char.toLower
        varNameTable table = mkName $ '_':(toLowerString table)
        varNameTableField table field = mkName $ '_':((toLowerString table) ++ ('_':(toLowerString field)))

        selectE = VarE $ mkName "select"
        fromE = VarE $ mkName "from"
        where_E = VarE $ mkName "where_"
        onE = VarE $ mkName "on"
        justE = VarE $ mkName "just"
        carotE = VarE $ mkName "^."
        questionE = VarE $ mkName "?."
        eqE = VarE $ mkName "==."
        geE = VarE $ mkName ">=."
        gE = VarE $ mkName ">."
        leE = VarE $ mkName "<=."
        lE = VarE $ mkName "<."
        innerJoin = VarE $ mkName "InnerJoin"
        leftOuterJoin = VarE $ mkName "LeftOuterJoin"
        rightOuterJoin = VarE $ mkName "RightOuterJoin"
        fullOuterJoin = VarE $ mkName "FullOuterJoin"
        -- crossJoin = VarE $ mkName "CrossJoin"

data ReqTerm = ReqField {
        reqFieldTable :: String
      , reqFieldField :: String
      , reqFieldIsMaybe :: Bool
      , reqFieldReturning :: Bool
      , reqFieldDependencies :: [(String,String)] -- Contains ( table, field) dependencies.
    }
  | ReqEntity {
        reqEntityTable :: String
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
data Select = Select | PSelect

--data Terms = Term Term | Terms Terms Term | TermsAll
data Terms = Terms [Term] | TermsAll

data Tables = Table String | Tables Tables Join String BExpr
data Join = InnerJoin | LeftOuterJoin | RightOuterJoin | FullOuterJoin -- | CrossJoin

data Where = Where BExpr

data OrderBy = OrderBy [Order]

data Order = OrderAsc Term | OrderDesc Term

data Limit = Limit Int64

data Offset = Offset Int64

data Term = TermTF String TermField | TermF TermField
data TermField = Field String | FieldAll

data BExpr = BExprAnd BExpr BExpr | BExprOr BExpr BExpr | BExprBinOp B BinOp B | BExprNull Term | BExprNotNull Term | BExprNot BExpr

data BinOp = BinEq | BinGE | BinG | BinLE | BinL

data B = BTerm Term | BAnti String | BConst C
data C = CBool Bool | CString String | CInt Int64 | CDouble Double

instance Show TermField where
    show (Field s) = s
    show (FieldAll) = "*"

parseCommand :: Parser Command
parseCommand = do
    select <- parseSelect
    terms <- parseTerms
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
                    return $ Tables acc join (Text.unpack table) bexpr
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

        parseBExpr = ( do
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
          ) <|> ( do
            expr1 <- parseBExpr
            skipSpace
            constr <- (asciiCI "AND" >> (return BExprAnd)) <|>
                (asciiCI "OR" >> (return BExprOr))
            expr2 <- parseBExpr
            return $ constr expr1 expr2
          )

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
                var <- takeNonSpace -- TODO: maybe make this into a [String] and stop at '}'
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

            parseBOp = skipSpace >> ( asciiCI "==" >> return BinEq) <|> 
                ( asciiCI ">=" >> return BinGE) <|>
                ( char '>' >> return BinG) <|>
                ( asciiCI "<=" >> return BinLE) <|>
                ( char '<' >> return BinL)

