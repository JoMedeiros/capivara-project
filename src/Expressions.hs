module Expressions where

import Tokens
import Lexer

import Text.Parsec
import Control.Monad.IO.Class

import System.IO.Unsafe

variable :: ParsecT [Token] [(Token,Token)] IO(Token)
variable =  (do 
                s <- getState
                a <- idToken
                return (getVal a s))
 
getVal :: Token -> [(Token, Token)] -> Token
getVal _ [] = error "variable not found"
getVal (Id id1 p1) ((Id id2 _, val):t) = if id1 == id2 then val
                                         else getVal (Id id1 p1) t

----------------------------------------
-- Expressions
----------------------------------------
-- @TODO Expand expressions
expression :: ParsecT [Token] [(Token,Token)] IO(Token)
expression = try bin_expression <|> una_expression

una_expression :: ParsecT [Token] [(Token,Token)] IO(Token)
una_expression =  (do
                      op <- plusToken
                      a <- intToken <|> variable
                      return (a))
 
--- funções considerando associatividade à esquerda                  
bin_expression :: ParsecT [Token] [(Token,Token)] IO(Token)
bin_expression = do
                   n1 <- intToken <|> variable
                   result <- eval_remaining n1
                   return (result)

eval_remaining :: Token -> ParsecT [Token] [(Token,Token)] IO(Token)
eval_remaining n1 = do
                      op <- plusToken
                      n2 <- intToken <|> variable
                      result <- eval_remaining (eval n1 op n2)
                      return (result) 
                    <|> return (n1)                              

-- Expression evaluation
eval :: Token -> Token -> Token -> Token
eval (Int x p) (Plus _ ) (Int y _) = Int (x + y) p

