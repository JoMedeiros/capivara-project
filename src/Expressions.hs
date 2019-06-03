module Expressions where

import Tokens
import Lexer

import Text.Parsec
import Control.Monad.IO.Class

import System.IO.Unsafe

----------------------------------------
-- Expressions
----------------------------------------
-- @TODO Expand expressions

expression = atomicExpr

atomicExpr :: ParsecT [Token] [(Token,Token)] IO([Token])
atomicExpr = do
          a <- intToken <|> floatToken <|> booleanToken <|> 
            charToken <|> stringToken
          return [a]

