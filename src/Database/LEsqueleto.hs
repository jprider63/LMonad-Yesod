{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Database.LEsqueleto (lsql) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Reader
import Data.Attoparsec.Text
import qualified Data.Char as Char
import Data.Int (Int64)
import qualified Data.Text as Text
import Database.LPersist
--import Database.Persist
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import LMonad
import Yesod.Persist.Core

lsql :: QuasiQuoter
lsql = QuasiQuoter {
        quoteExp = generateSql . Text.pack
    }

--parse :: (Label l, PersistConfig c, LMonad m, m ~ HandlerT site IO) => Text -> PersistConfigBackend c (LMonadT l m) b
--generateSql :: (Label l, m ~ HandlerT site IO, YesodLPersist site) => ReaderT (YesodPersistBackend site) (LMonadT l m) a 
generateSql s = 
    let ast = case parseOnly parseCommand s of
          Left err ->
            error $ "Error parsing lsql statement: " ++ err
          Right res ->
            res
    in
    undefined

data Command = Command Select Terms Tables (Maybe Where) (Maybe OrderBy) (Maybe Limit) (Maybe Offset)
data Select = Select | PSelect

--data Terms = Term Term | Terms Terms Term | TermsAll
data Terms = Terms [Term] | TermsAll

data Tables = Table String | Tables Tables Join String BExpr
data Join = InnerJoin | LeftOuterJoin | RightOuterJoin | FullOuterJoin | CrossJoin

data Where = Where BExpr

data OrderBy = OrderBy [Order]

data Order = OrderAsc Term | OrderDesc Term

data Limit = Limit Int64

data Offset = Offset Int64

data Term = TermTF String TermField | TermF TermField
data TermField = Field String | FieldAll

data BExpr = BExprAnd BExpr BExpr | BExprOr BExpr BExpr | BExprBinOP B BinOp B | BExprNull Term | BExprNotNull Term | BExprNot BExpr

data BinOp = BinEq | BinGT | BinG | BinLT | BinL

data B = BTerm Term | BAnti String | BConst Int64

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
                      ( asciiCI "FULL OUTER JOIN" >> (return FullOuterJoin)) <|>
                      ( asciiCI "CROSS JOIN" >> (return CrossJoin))
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
            _ <- char "("
            res <- parseBExpr
            skipSpace
            _ <- char ")"
            return res
          ) <|> ( do
            skipSpace
            _ <- asciiCI "NOT"
            res <- parseBExpr
            return $ BExprNot res
          ) <|> ( do
            term <- parseTerm
            skipSpace
            asciiCI "IS NULL"
            return $ BExprNull term
          ) <|> ( do
            term <- parseTerm
            skipSpace
            asciiCI "IS NOT NULL"
          ) <|> ( do
            b1 <- parseB
            constr <- parseBConstr
            b2 <- parseB
            return $ cosntr b1 b2
          ) <|> ( do
            expr1 <- parseBExpr
            skipSpace
            constr <- (asciiCI "AND" >> (return BExprAnd)) <|>
                (asciiCI "OR" >> (return BExprOr))
            expr2 <- parseBExpr
            return $ constr expr1 expr2
          )

        where
            parseB = undefined
            parseBConstr = undefined

