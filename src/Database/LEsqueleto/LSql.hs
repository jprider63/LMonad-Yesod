{-# LANGUAGE OverloadedStrings #-}

module Database.LEsqueleto.LSql where

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Char as Char
import qualified Data.Text as Text

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
    deriving (Eq, Show)

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
    parseComma
    return $ Command select terms tables whereM orderByM limitM offsetM

    where
        takeNonSpace = takeWhile1 (not . Char.isSpace)

        takeAlphaNum = takeWhile1 Char.isAlphaNum
        takeUpperAlphaNum = do
            an <- takeAlphaNum
            return $ Text.map Char.toUpper an

        parseComma = do
            skipSpace
            -- Check for optional comma.
            commaM <- peekChar
            case commaM of
                Nothing ->
                    return ()
                Just c ->
                    if c == ';' then
                        return ()
                    else do
                        rest <- takeText
                        error $ "Error parsing from: `" ++ (Text.unpack rest) ++ "`"
            -- maybe (return ()) 
            -- try (char ';' >> skipSpace)
            -- end <- atEnd
            -- unless end $ do
            --     rest <- takeText
            --     error $ "Error parsing from: `"++ (Text.unpack rest) ++"`"

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
                      ) <|> (return [])
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
                        if Char.isSpace c then
                            return BExprOr
                        else
                            fail "OR: Some other keyword"
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
