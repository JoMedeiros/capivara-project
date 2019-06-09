module Expressions where

import Tokens
import Lexer
import SymTable

import Text.Parsec
import Control.Monad.IO.Class

import System.IO.Unsafe

variable :: ParsecT [Token] [MemCell] IO(Token)
variable =  (do 
                s <- getState
                a <- idToken
                return (getVal a s))
 
getVal :: Token -> [MemCell] -> Token
getVal (Id _ (l, c)) [] = error ("variable not declared in the scope at line " ++ (show l) ++ " column " ++ (show c))
getVal (Id id1 p1) ((Var (Id id2 _, val)):t) = if id1 == id2 then val
                                         else getVal (Id id1 p1) t

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
expression :: ParsecT [Token] [MemCell] IO(Token)
expression = try bin_expression <|> una_expression

una_expression :: ParsecT [Token] [MemCell] IO(Token)
una_expression =  (do
                      op <- plusToken <|> minusToken
                      a <- term1
                      return (a))
 
--- funções considerando associatividade à esquerda                  
bin_expression :: ParsecT [Token] [MemCell] IO(Token)
bin_expression = do
                   n1 <- term1
                   result <- eval_remaining n1
                   return (result)

eval_remaining :: Token -> ParsecT [Token] [MemCell] IO(Token)
eval_remaining n1 = do
                      op <- plusToken <|> minusToken
                      n2 <- term1
                      result <- eval_remaining (eval n1 op n2)
                      return (result) 
                    <|> return (n1)                              

--------------------- Level 2 --------------------
term1 :: ParsecT [Token] [MemCell] IO(Token)
term1 = try bin_term1 <|> una_term1

una_term1 :: ParsecT [Token] [MemCell] IO(Token)
una_term1 =  (do
                op <- multToken <|> divToken <|> modToken
                a <- term2
                return (a))
 
--- funções considerando associatividade à esquerda                  
bin_term1 :: ParsecT [Token] [MemCell] IO(Token)
bin_term1 = do
                   n1 <- term2
                   result <- eval_term1 n1
                   return (result)

eval_term1 :: Token -> ParsecT [Token] [MemCell] IO(Token)
eval_term1 n1 = do
                    op <- multToken <|> divToken <|> modToken
                    n2 <- term2
                    result <- eval_term1 (eval n1 op n2)
                    return (result) 
                    <|> return (n1)                              

--------------------- Level 3 --------------------
term2 :: ParsecT [Token] [MemCell] IO(Token)
term2 = try bin_term2 <|> una_term2

una_term2 :: ParsecT [Token] [MemCell] IO(Token)
una_term2 =  (do
                op <- powerToken
                a <- intToken <|> variable
                return (a))
 
--- funções considerando associatividade à esquerda                  
bin_term2 :: ParsecT [Token] [MemCell] IO(Token)
bin_term2 = do
                   n1 <- intToken <|> variable
                   result <- eval_term2 n1
                   return (result)

eval_term2 :: Token -> ParsecT [Token] [MemCell] IO(Token)
eval_term2 n1 = do
                    op <- powerToken
                    n2 <- intToken <|> variable
                    result <- eval_term2 (eval n1 op n2)
                    return (result) 
                    <|> return (n1)                              

-- Expression evaluation
eval :: Token -> Token -> Token -> Token
eval (Int x p) (Plus _ ) (Int y _) = Int (x + y) p
eval (Int x p) (Minus _ ) (Int y _) = Int (x - y) p
eval (Int x p) (Mult _ ) (Int y _) = Int (x * y) p
eval (Int x (l,c)) (Div _ ) (Int 0 _) = error ("Error: Division by zero on line " ++ (show l) ++ " column " ++ (show c))
eval (Int x p) (Div _ ) (Int y _) = Int (x `div` y) p
eval (Int x (l,c)) (Mod _ ) (Int 0 _) = error ("Error: Division by zero on line " ++ (show l) ++ " column " ++ (show c))
eval (Int x p) (Mod _ ) (Int y _) = Int (x `mod` y) p
eval (Int x p) (Power _ ) (Int y _) = Int (x ^ y) p

