module Lexer where

import Tokens
----------------------------------------
-- Tokens
----------------------------------------

import Text.Parsec
import Control.Monad.IO.Class

import System.IO.Unsafe

----------------------------------------
-- Tokens
----------------------------------------

beginToken :: ParsecT [Token] st IO (Token)
beginToken = tokenPrim show update_pos get_token where
  get_token Begin = Just Begin
  get_token _     = Nothing

programToken :: ParsecT [Token] st IO (Token)
programToken = tokenPrim show update_pos get_token where
  get_token Program = Just Program
  get_token _       = Nothing

endToken :: ParsecT [Token] st IO (Token)
endToken = tokenPrim show update_pos get_token where
  get_token End = Just End
  get_token _   = Nothing

idToken :: ParsecT [Token] st IO (Token)
idToken = tokenPrim show update_pos get_token where
  get_token (Id x) = Just (Id x)
  get_token _      = Nothing

semiColonToken :: ParsecT [Token] st IO (Token)
semiColonToken = tokenPrim show update_pos get_token where
  get_token SemiColon = Just SemiColon
  get_token _         = Nothing

--colonToken = tokenPrim show update_pos get_token where
--  get_token Colon = Just Colon
--  get_token _     = Nothing

assignToken :: ParsecT [Token] st IO (Token)
assignToken = tokenPrim show update_pos get_token where
  get_token Assign = Just Assign
  get_token _      = Nothing

constToken :: ParsecT [Token] st IO (Token)
constToken = tokenPrim show update_pos get_token where
  get_token Const   = Just Const
  get_token _       = Nothing

functionToken :: ParsecT [Token] st IO (Token)
functionToken = tokenPrim show update_pos get_token where
  get_token Function   = Just Function
  get_token _       = Nothing

beginBracketToken :: ParsecT [Token] st IO (Token)
beginBracketToken = tokenPrim show update_pos get_token where
  get_token BeginExp = Just BeginExp
  get_token _        = Nothing

endBracketToken :: ParsecT [Token] st IO (Token)
endBracketToken = tokenPrim show update_pos get_token where
  get_token EndExp = Just BeginExp
  get_token _      = Nothing

intToken :: ParsecT [Token] st IO (Token)
intToken = tokenPrim show update_pos get_token where
  get_token (Int x) = Just (Int x)
  get_token _       = Nothing

typeToken :: ParsecT [Token] st IO (Token)
typeToken = tokenPrim show update_pos get_token where
  get_token (Type x) = Just (Type x)
  get_token _        = Nothing 

----------------------------------------
--  Auxiliary Functions
----------------------------------------

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- needs improvement
update_pos pos _ []      = pos  

