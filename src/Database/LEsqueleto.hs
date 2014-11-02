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
data Join = InnerJoin | OuterJoin | LeftOuterJoin | RightOuterJoin | FullOuterJoin

data Where = Where BExpr

data OrderBy = OrderBy Order

data Order = OrderT Term | OrderTA Term | OrderTD Term | OrderTO Term Order

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

        parseTables = undefined

        parseWhere = ( do
            skipSpace
            _ <- asciiCI "WHERE"
            bexp <- parseBExpr
            return $ Just $ Where bexp
          ) <|> (return Nothing)
        
        parseOrderBy = ( do
            skipSpace
            _ <- asciiCI "ORDER BY"
            order <- parseOrder
            return $ Just $ OrderBy order
          ) <|> (return Nothing)

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

        parseBExpr = undefined
        parseOrder = undefined


