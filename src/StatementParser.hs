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
            e <- semicolonToken
            s <- getState
            return (a:b:[e])

ifStatement :: ParsecT [Token] CapivaraState IO([Token])
ifStatement = do
            ws <- getInput 
            f <- ifToken
            a <- beginbracketToken
            b <- expression
            c <- endbracketToken 
            d <- block
            return (f:a:b:c:d)

-------------------- While --------------------
whileStatement :: ParsecT [Token] CapivaraState IO([Token])
whileStatement = do
            ws <- getInput 
            f <- whileToken
            a <- beginbracketToken
            b <- expression
            c <- endbracketToken
            d <- block
            return []

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

identifiersList = (do
                    a <- idToken
                    b <- commaToken
                    c <- identifiersList
                    return (a:b:c)) <|>
                  (do
                    a <- idToken
                    b <- identifiersList
                    return (a:b)) <|> (return [])

stmts :: ParsecT [Token] CapivaraState IO([Token])
stmts = (do a <- assign <|> varDecl <|> block <|> capivaraWrite <|> capivaraRead <|> whileStatement <|> ifStatement
            b <- stmts
            return (a ++ b)) <|> (return [])

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
          return []

-- TODO fazer funções para "printar" cada tipo
capivaraWrite :: ParsecT [Token] CapivaraState IO([Token])
capivaraWrite = do
                a <- writeToken
                b <- expression
                c <- semicolonToken
                return (a:b:[c])

-- token2str :: Token -> String
-- token2str (Int v p) = show v
-- token2str (Float v p) = show v
-- token2str (Char v p) = show v
-- token2str (Boolean v p) = show v
-- token2str (String v p) = read v
-- token2str t = show t

capivaraRead :: ParsecT [Token] CapivaraState IO([Token])
capivaraRead = do 
               a <- readToken
               b <- greaterToken
               c <- idToken
               d <- semicolonToken
               return ([a])

-- generateToken :: Token -> String -> Token
-- generateToken (String _ p ) s = (String s p)
-- generateToken (Int _ p) s = (Int (read (read s)) p)
-- generateToken (Float _ p) s = (Float (read (read s)) p)
-- generateToken (Char _ p) s = (Char (read (read s)) p)
-- generateToken (Boolean _ p) s = (Boolean (read (read s)) p)
-- generateToken _ _ = error "error: input type error"

-- invocação do parser para o símbolo de partida 

-- main :: IO ()
-- main = do
--        (file:args) <-getArgs
--        putStrLn file
--        case unsafePerformIO (parser (getTokens file)) of
--             { Left err -> print err; 
--               Right ans -> print ans
--             }

variable :: ParsecT [Token] CapivaraState IO(Token)
variable =  (do 
                s <- getState
                a <- idToken
                return (getVal a (
                    getCurrentScope(s))
                  )
             )
 
getVal :: Token -> Scope -> Token
getVal (Id _ (l, c)) (_,_,[]) = error ("variable not declared in the scope at line " ++ (show l) ++ " column " ++ (show c))
getVal (Id id1 p1) (i,j,((Id id2 _,val)):t) = if id1 == id2 then val
                                         else getVal (Id id1 p1) (i,j,t)

----------------------------------------
-- Expressions
----------------------------------------
-- @TODO Expand expressions
-- <bool_expression> = <term-1>, { [ ( “and” | “or” | “xor” ), <term-1> ] };
-- <term-1> = <expression>, { [ ( “==” | “!=” | “<” | “>” | “<=” | “>=” ), <expression> ] };
-- <expression> = <term-3>, { [ ( “+” | “-” ), <term-3> ] };
-- <term-3> = <term-4>, { [ ( “*” | “/” | “mod” ), <term-4> ] };
-- <term-4> = <factor>, { [ ( “**” ), <factor> ] } | ( “++” | “--” ), <term-1>;
-- <factor> = identifier | <literal> | “(”, <expression>, “)”;
-- <literal> = int-literal | float-literal | boolean-literal | string-literal

--------------------- Level 1 --------------------
expression :: ParsecT [Token] CapivaraState IO(Token)
expression = try bin_expression <|> una_expression

una_expression :: ParsecT [Token] CapivaraState IO(Token)
una_expression =  (do
                      op <- opandToken <|> oporToken <|> opxorToken
                      a <- term1
                      return (a))

--- funções considerando associatividade à esquerda                  
bin_expression :: ParsecT [Token] CapivaraState IO(Token)
bin_expression = do
                   n1 <- term1
                   result <- eval_remaining n1
                   return (result)

eval_remaining :: Token -> ParsecT [Token] CapivaraState IO(Token)
eval_remaining n1 = do
                      op <- opandToken <|> oporToken <|> opxorToken
                      n2 <- term1
                      result <- eval_remaining (n1)
                      return (result) 
                    <|> return (n1)         

--------------------- Level 2 --------------------
term1 :: ParsecT [Token] CapivaraState IO(Token)
term1 = try bin_term1 <|> una_term1

una_term1 :: ParsecT [Token] CapivaraState IO(Token)
una_term1 =  (do
                      op <- equalToken <|> differentToken <|> greaterToken <|> lessToken <|> greaterorequalToken <|> lessorequalToken
                      a <- term2
                      return (a))

--- funções considerando associatividade à esquerda                  
bin_term1 :: ParsecT [Token] CapivaraState IO(Token)
bin_term1 = do
                   n1 <- term2
                   result <- eval_term1 n1
                   return (result)     

eval_term1 :: Token -> ParsecT [Token] CapivaraState IO(Token)
eval_term1 n1 = do
                    op <- equalToken <|> differentToken <|> greaterToken <|> lessToken <|> greaterorequalToken <|> lessorequalToken
                    n2 <- term2
                    result <- eval_term1 (n1)
                    return (result) 
                    <|> return (n1)

--------------------- Level 3 --------------------
term2 :: ParsecT [Token] CapivaraState IO(Token)
term2 = try bin_term2 <|> una_term2

una_term2 :: ParsecT [Token] CapivaraState IO(Token)
una_term2 =  (do
                      op <- plusToken <|> minusToken
                      a <- term3
                      return (a))

--- funções considerando associatividade à esquerda                  
bin_term2 :: ParsecT [Token] CapivaraState IO(Token)
bin_term2 = do
                   n1 <- term3
                   result <- eval_term2 n1
                   return (result)             

eval_term2 :: Token -> ParsecT [Token] CapivaraState IO(Token)
eval_term2 n1 = do
                    op <- plusToken <|> minusToken
                    n2 <- term3
                    result <- eval_term2 (n1)
                    return (result) 
                    <|> return (n1)

--------------------- Level 4 --------------------
term3 :: ParsecT [Token] CapivaraState IO(Token)
term3 = try bin_term3 <|> una_term3

una_term3 :: ParsecT [Token] CapivaraState IO(Token)
una_term3 =  (do
                op <- multToken <|> divToken <|> modToken
                a <- term4
                return (a))
 
--- funções considerando associatividade à esquerda                  
bin_term3 :: ParsecT [Token] CapivaraState IO(Token)
bin_term3 = do
                   n1 <- term4
                   result <- eval_term3 n1
                   return (result)

eval_term3 :: Token -> ParsecT [Token] CapivaraState IO(Token)
eval_term3 n1 = do
                    op <- multToken <|> divToken <|> modToken
                    n2 <- term4
                    result <- eval_term3 (n1)
                    return (result) 
                    <|> return (n1)                              

--------------------- Level 5 --------------------
term4 :: ParsecT [Token] CapivaraState IO(Token)
term4 = try bin_term4 <|> una_term4

una_term4 :: ParsecT [Token] CapivaraState IO(Token)
una_term4 =  (do
                op <- powerToken
                a <- intToken <|> booleanToken <|> floatToken <|> stringToken <|> variable 
                return (a))
 
--- funções considerando associatividade à esquerda                  
bin_term4 :: ParsecT [Token] CapivaraState IO(Token)
bin_term4 = do
                   n1 <- intToken <|> booleanToken <|> floatToken <|> stringToken <|> variable
                   result <- eval_term4 n1
                   return (result)

eval_term4 :: Token -> ParsecT [Token] CapivaraState IO(Token)
eval_term4 n1 = do
                    op <- powerToken
                    n2 <- intToken <|> booleanToken  <|> floatToken <|> stringToken <|> variable 
                    result <- eval_term4 (n1)
                    return (result) 
                    <|> return (n1)                              


