-- module Main (main) where
module StatementParser where

import Tokens
import Lexer
import Text.Parsec
import Control.Monad.IO.Class
import SymTable

import Text.Parsec
import Control.Monad.IO.Class

import System.IO.Unsafe

-- EBNF based:

voidTokens :: ParsecT [Token] CapivaraState IO([Token])
voidTokens =  do
            return []

varDecl :: ParsecT [Token] CapivaraState IO([Token])
varDecl = do
            a <- typeToken
            b <- idToken
            c <- semicolonToken
            return (a:b:[c])

ifStatement :: ParsecT [Token] CapivaraState IO([Token])
ifStatement = do
            f <- ifToken
            a <- beginbracketToken
            b <- expression
            c <- endbracketToken 
            d <- block
            try (do 
              e <- elseToken
              g <- block
              return (f:a:b ++ c:d ++ e:g)) <|> (do return (f:a:b ++ c:d))
            return (f:a:b ++ c:d)

-------------------- While --------------------
whileStatement :: ParsecT [Token] CapivaraState IO([Token])
whileStatement = do
            f <- whileToken
            a <- beginbracketToken
            b <- expression
            c <- endbracketToken
            d <- block
            return (f:a:b ++ c:d)

-------------------- /While --------------------

{-
forStatement :: ParsecT [Token] CapivaraState IO([Token])
forStatement = do
            f <- forToken
            a <- beginbracketToken
            
            b <- 

            c <- semicolonToken
            d <- expression
            e <- semicolonToken
            g <- expression
            h <- endbracketToken
            i <- expression
            j <- block
            return (f:a:b:c:d:e:g:h:i:j)            
-}
function :: ParsecT [Token] CapivaraState IO([Token])
function = do
            f <- functionToken
            a <- typeToken
            b <- idToken
            c <- beginbracketToken
            p <- paramsList
            d <- endbracketToken
            e <- block
            return (f:a:b:c:d:e)

procedure :: ParsecT [Token] CapivaraState IO([Token])
procedure = do
            f <- procedureToken
            b <- idToken
            c <- beginbracketToken
            p <- paramsList
            d <- endbracketToken
            e <- block
            return (f:b:c:p ++ (d:e))

paramsList :: ParsecT [Token] CapivaraState IO([Token])
paramsList = (do
               a <- typeToken
               b <- idToken
               c <- paramsList
               return (a:b:c)) <|>
             (do
               a <- commaToken
               b <- typeToken
               c <- idToken
               d <- paramsList
               return (a:b:c:d)) <|> (return [])

argsList = (do
               a <- expression
               b <- commaToken
               c <- argsList
               return (a ++ b:c)) <|>
             (do
               a <- expression
               b <- argsList
               return (a ++ b)) <|> (return [])

stmts :: ParsecT [Token] CapivaraState IO([Token])
stmts = try (do a <- assign <|> varDecl <|> block <|> capivaraWrite <|> 
                    capivaraRead <|> whileStatement <|> ifStatement <|> returnExp
                b <- stmts
                return (a ++ b)) <|> 
            (do a <- functionCall
                b <- stmts
                return (a ++ b)) <|> (return [])

functionCall :: ParsecT [Token] CapivaraState IO([Token])
functionCall = do
          z <- lessToken
          a <- idToken
          b <- beginbracketToken
          c <- argsList
          d <- endbracketToken
          w <- greaterToken
          return (z:a:b:c ++ d:[w])

returnExp :: ParsecT [Token] CapivaraState IO([Token])
returnExp = do
            a <- returnToken
            b <- expression
            c <- semicolonToken
            return (a:b ++ [c])

block :: ParsecT [Token] CapivaraState IO([Token])
block = (do a <- beginscopeToken
            b <- stmts
            c <- endscopeToken
            return (a:b ++ [c]))

assign :: ParsecT [Token] CapivaraState IO([Token])
assign = do
          a <- idToken
          b <- assignToken
          c <- expression
          d <- semicolonToken
          return (a:b:c ++ [d])

-- TODO fazer funções para "printar" cada tipo
capivaraWrite :: ParsecT [Token] CapivaraState IO([Token])
capivaraWrite = do
                a <- writeToken
                b <- expression
                c <- semicolonToken
                return (a:b ++ [c])

capivaraRead :: ParsecT [Token] CapivaraState IO([Token])
capivaraRead = do 
               a <- readToken
               b <- greaterToken
               c <- idToken
               d <- semicolonToken
               return (a:b:c:[d])

-- invocação do parser para o símbolo de partida 

----------------------------------------
-- Expressions
----------------------------------------
--------------------- Level 1 --------------------
expression :: ParsecT [Token] CapivaraState IO([Token])
expression = try bin_expression <|> una_expression

una_expression :: ParsecT [Token] CapivaraState IO([Token])
una_expression =  (do
                      op <- opandToken <|> oporToken <|> opxorToken
                      a <- term1
                      return (op:a))

--- funções considerando associatividade à esquerda                  
bin_expression :: ParsecT [Token] CapivaraState IO([Token])
bin_expression = do
                   n1 <- term1
                   result <- eval_remaining
                   return (n1 ++ result)

eval_remaining :: ParsecT [Token] CapivaraState IO([Token])
eval_remaining = do
                      op <- opandToken <|> oporToken <|> opxorToken
                      n2 <- term1
                      result <- eval_remaining
                      return (op:n2 ++ result) 
                    <|> return []         

--------------------- Level 2 --------------------
term1 :: ParsecT [Token] CapivaraState IO([Token])
term1 = try bin_term1 <|> una_term1

una_term1 :: ParsecT [Token] CapivaraState IO([Token])
una_term1 =  (do
                 op <- equalToken <|> differentToken <|> greaterToken <|> 
                    lessToken <|> greaterorequalToken <|> lessorequalToken
                 a <- term2
                 return (op:a))

--- funções considerando associatividade à esquerda                  
bin_term1 :: ParsecT [Token] CapivaraState IO([Token])
bin_term1 = do
               n1 <- term2
               result <- eval_term1
               return (n1 ++ result)     

eval_term1 :: ParsecT [Token] CapivaraState IO([Token])
eval_term1 = do
                op <- equalToken <|> differentToken <|> greaterToken <|> 
                   lessToken <|> greaterorequalToken <|> lessorequalToken
                n2 <- term2
                result <- eval_term1
                return (op:n2 ++ result) 
                <|> return []

--------------------- Level 3 --------------------
term2 :: ParsecT [Token] CapivaraState IO([Token])
term2 = try bin_term2 <|> una_term2

una_term2 :: ParsecT [Token] CapivaraState IO([Token])
una_term2 =  (do
                 op <- plusToken <|> minusToken
                 a <- term3
                 return (op:a))

--- funções considerando associatividade à esquerda                  
bin_term2 :: ParsecT [Token] CapivaraState IO([Token])
bin_term2 = do
               n1 <- term3
               result <- eval_term2
               return (n1 ++ result)             

eval_term2 :: ParsecT [Token] CapivaraState IO([Token])
eval_term2 = do
                op <- plusToken <|> minusToken
                n2 <- term3
                result <- eval_term2
                return (op:n2 ++ result) 
                <|> return []

--------------------- Level 4 --------------------
term3 :: ParsecT [Token] CapivaraState IO([Token])
term3 = try bin_term3 <|> una_term3

una_term3 :: ParsecT [Token] CapivaraState IO([Token])
una_term3 =  (do
                op <- multToken <|> divToken <|> modToken
                a <- term4
                return (op:a))
 
--- funções considerando associatividade à esquerda                  
bin_term3 :: ParsecT [Token] CapivaraState IO([Token])
bin_term3 = do
                   n1 <- term4
                   result <- eval_term3
                   return (n1 ++ result)

eval_term3 :: ParsecT [Token] CapivaraState IO([Token])
eval_term3 = (do
                op <- multToken <|> divToken <|> modToken
                n2 <- term4
                result <- eval_term3
                return (op:n2 ++ result))
                <|> (do return [])

-------------------------- atualizado begin
--------------------- Level 5 --------------------
term4 :: ParsecT [Token] CapivaraState IO([Token])
term4 = try bin_term4 <|> una_term4

una_term4 :: ParsecT [Token] CapivaraState IO([Token])
una_term4 =  try (do
                op <- powerToken <|> plusplusToken
                a <- intToken <|> booleanToken <|> floatToken <|> stringToken
                    <|> idToken
                return (op:[a])) <|>
                (do 
                  op <- powerToken <|> plusplusToken
                  a <- try listLiteral <|> matrixLiteral 
                      <|> parenExpr
                  return (op:a))
 
--- funções considerando associatividade à esquerda                  
bin_term4 :: ParsecT [Token] CapivaraState IO([Token])
bin_term4 = (do
               n1 <- intToken <|> booleanToken <|> floatToken <|> 
                  stringToken <|> idToken 
               result <- eval_term4
               return (n1:result)) <|>
            (do
               n1 <- try listLiteral <|> matrixLiteral <|> parenExpr
               result <- eval_term4
               return (n1 ++ result))

eval_term4 :: ParsecT [Token] CapivaraState IO([Token])
eval_term4 = (do
                   op <- powerToken <|> plusplusToken
                   n2 <- intToken <|> booleanToken  <|> floatToken <|> 
                      stringToken <|> idToken
                   result <- eval_term4
                   return (op:n2:result)
                   <|> return []) <|>
                (do
                   op <- powerToken <|> plusplusToken
                   n2 <- try listLiteral <|> matrixLiteral <|> parenExpr
                   result <- eval_term4
                   return (op:n2 ++ result)
                   <|> return [])

parenExpr :: ParsecT [Token] CapivaraState IO([Token])
parenExpr = do
               a <- beginbracketToken
               b <- expression
               c <- endbracketToken
               return (a:b ++ [c])

listLiteral :: ParsecT [Token] CapivaraState IO([Token])
listLiteral = (do
                a <- beginlistToken
                b <- manyTill (intToken <|> booleanToken  <|> 
                      floatToken <|> stringToken) (endlistToken) <|>
                      manyTill idToken (endlistToken) <|> (return [])
                return (a:b))

matrixLiteral :: ParsecT [Token] CapivaraState IO([Token])
matrixLiteral = (do
                a <- beginlistToken
                b <- listLiterals
                c <- endlistToken
                return (a:b ++ [c]))

listLiterals :: ParsecT [Token] CapivaraState IO([Token])
listLiterals = (do
                  a <- listLiteral
                  b <- listLiterals
                  return (a ++ b)) <|>
                (do return [])

--------------------------- atualizado end

