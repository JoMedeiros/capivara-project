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

programToken :: ParsecT [Token] st IO(Token)
programToken = tokenPrim show update_pos get_token where
  get_token (Program p) = Just (Program p)
  get_token _ = Nothing

beginToken :: ParsecT [Token] st IO(Token)
beginToken = tokenPrim show update_pos get_token where
  get_token (Begin p) = Just (Begin p)
  get_token _ = Nothing

endToken :: ParsecT [Token] st IO(Token)
endToken = tokenPrim show update_pos get_token where
  get_token (End p) = Just (End p)
  get_token _ = Nothing

colonToken :: ParsecT [Token] st IO(Token)
colonToken = tokenPrim show update_pos get_token where
  get_token (Colon p) = Just (Colon p)
  get_token _ = Nothing

semicolonToken :: ParsecT [Token] st IO(Token)
semicolonToken = tokenPrim show update_pos get_token where
  get_token (SemiColon p) = Just (SemiColon p)
  get_token _ = Nothing

commaToken :: ParsecT [Token] st IO(Token)
commaToken = tokenPrim show update_pos get_token where
  get_token (Comma p) = Just (Comma p)
  get_token _ = Nothing

assignToken :: ParsecT [Token] st IO(Token)
assignToken = tokenPrim show update_pos get_token where
  get_token (Assign p) = Just (Assign p)
  get_token _ = Nothing

constToken :: ParsecT [Token] st IO(Token)
constToken = tokenPrim show update_pos get_token where
  get_token (Const p) = Just (Const p)
  get_token _ = Nothing

functionToken :: ParsecT [Token] st IO(Token)
functionToken = tokenPrim show update_pos get_token where
  get_token (Function p) = Just (Function p)
  get_token _ = Nothing

procedureToken :: ParsecT [Token] st IO(Token)
procedureToken = tokenPrim show update_pos get_token where
  get_token (Procedure p) = Just (Procedure p)
  get_token _ = Nothing

ifToken :: ParsecT [Token] st IO(Token)
ifToken = tokenPrim show update_pos get_token where
  get_token (If p) = Just (If p)
  get_token _ = Nothing

thenToken :: ParsecT [Token] st IO(Token)
thenToken = tokenPrim show update_pos get_token where
  get_token (Then p) = Just (Then p)
  get_token _ = Nothing

writeToken :: ParsecT [Token] st IO(Token)
writeToken = tokenPrim show update_pos get_token where
  get_token (Write p) = Just (Write p)
  get_token _ = Nothing

beginscopeToken :: ParsecT [Token] st IO(Token)
beginscopeToken = tokenPrim show update_pos get_token where
  get_token (BeginScope p) = Just (BeginScope p)
  get_token _ = Nothing

endscopeToken :: ParsecT [Token] st IO(Token)
endscopeToken = tokenPrim show update_pos get_token where
  get_token (EndScope p) = Just (EndScope p)
  get_token _ = Nothing

beginbracketToken :: ParsecT [Token] st IO(Token)
beginbracketToken = tokenPrim show update_pos get_token where
  get_token (BeginBracket p) = Just (BeginBracket p)
  get_token _ = Nothing

endbracketToken :: ParsecT [Token] st IO(Token)
endbracketToken = tokenPrim show update_pos get_token where
  get_token (EndBracket p) = Just (EndBracket p)
  get_token _ = Nothing

beginlistToken :: ParsecT [Token] st IO(Token)
beginlistToken = tokenPrim show update_pos get_token where
  get_token (BeginList p) = Just (BeginList p)
  get_token _ = Nothing

endlistToken :: ParsecT [Token] st IO(Token)
endlistToken = tokenPrim show update_pos get_token where
  get_token (EndList p) = Just (EndList p)
  get_token _ = Nothing

plusplusToken :: ParsecT [Token] st IO(Token)
plusplusToken = tokenPrim show update_pos get_token where
  get_token (PlusPlus p) = Just (PlusPlus p)
  get_token _ = Nothing

plusToken :: ParsecT [Token] st IO(Token)
plusToken = tokenPrim show update_pos get_token where
  get_token (Plus p) = Just (Plus p)
  get_token _ = Nothing

minusminusToken :: ParsecT [Token] st IO(Token)
minusminusToken = tokenPrim show update_pos get_token where
  get_token (MinusMinus p) = Just (MinusMinus p)
  get_token _ = Nothing

minusToken :: ParsecT [Token] st IO(Token)
minusToken = tokenPrim show update_pos get_token where
  get_token (Minus p) = Just (Minus p)
  get_token _ = Nothing

multToken :: ParsecT [Token] st IO(Token)
multToken = tokenPrim show update_pos get_token where
  get_token (Mult p) = Just (Mult p)
  get_token _ = Nothing

divToken :: ParsecT [Token] st IO(Token)
divToken = tokenPrim show update_pos get_token where
  get_token (Div p) = Just (Div p)
  get_token _ = Nothing

modToken :: ParsecT [Token] st IO(Token)
modToken = tokenPrim show update_pos get_token where
  get_token (Mod p) = Just (Mod p)
  get_token _ = Nothing

powerToken :: ParsecT [Token] st IO(Token)
powerToken = tokenPrim show update_pos get_token where
  get_token (Power p) = Just (Power p)
  get_token _ = Nothing

elifToken :: ParsecT [Token] st IO(Token)
elifToken = tokenPrim show update_pos get_token where
  get_token (Elif p) = Just (Elif p)
  get_token _ = Nothing

elseToken :: ParsecT [Token] st IO(Token)
elseToken = tokenPrim show update_pos get_token where
  get_token (Else p) = Just (Else p)
  get_token _ = Nothing

switchToken :: ParsecT [Token] st IO(Token)
switchToken = tokenPrim show update_pos get_token where
  get_token (Switch p) = Just (Switch p)
  get_token _ = Nothing

caseToken :: ParsecT [Token] st IO(Token)
caseToken = tokenPrim show update_pos get_token where
  get_token (Case p) = Just (Case p)
  get_token _ = Nothing

oporToken :: ParsecT [Token] st IO(Token)
oporToken = tokenPrim show update_pos get_token where
  get_token (OpOr p) = Just (OpOr p)
  get_token _ = Nothing

opxorToken :: ParsecT [Token] st IO(Token)
opxorToken = tokenPrim show update_pos get_token where
  get_token (OpXor p) = Just (OpXor p)
  get_token _ = Nothing

opandToken :: ParsecT [Token] st IO(Token)
opandToken = tokenPrim show update_pos get_token where
  get_token (OpAnd p) = Just (OpAnd p)
  get_token _ = Nothing

equalToken :: ParsecT [Token] st IO(Token)
equalToken = tokenPrim show update_pos get_token where
  get_token (Equal p) = Just (Equal p)
  get_token _ = Nothing

differentToken :: ParsecT [Token] st IO(Token)
differentToken = tokenPrim show update_pos get_token where
  get_token (Different p) = Just (Different p)
  get_token _ = Nothing

greaterToken :: ParsecT [Token] st IO(Token)
greaterToken = tokenPrim show update_pos get_token where
  get_token (Greater p) = Just (Greater p)
  get_token _ = Nothing

lessToken :: ParsecT [Token] st IO(Token)
lessToken = tokenPrim show update_pos get_token where
  get_token (Less p) = Just (Less p)
  get_token _ = Nothing

greaterorequalToken :: ParsecT [Token] st IO(Token)
greaterorequalToken = tokenPrim show update_pos get_token where
  get_token (GreaterOrEqual p) = Just (GreaterOrEqual p)
  get_token _ = Nothing

lessorequalToken :: ParsecT [Token] st IO(Token)
lessorequalToken = tokenPrim show update_pos get_token where
  get_token (LessOrEqual p) = Just (LessOrEqual p)
  get_token _ = Nothing

typeToken :: ParsecT [Token] st IO (Token)
typeToken = tokenPrim show update_pos get_token where
  get_token (Type x p) = Just (Type x p)
  get_token _        = Nothing 

idToken :: ParsecT [Token] st IO (Token)
idToken = tokenPrim show update_pos get_token where
  get_token (Id x p) = Just (Id x p)
  get_token _      = Nothing

intToken :: ParsecT [Token] st IO (Token)
intToken = tokenPrim show update_pos get_token where
  get_token (Int x p) = Just (Int x p)
  get_token _       = Nothing

floatToken :: ParsecT [Token] st IO (Token)
floatToken = tokenPrim show update_pos get_token where
  get_token (Float x p) = Just (Float x p)
  get_token _       = Nothing

booleanToken :: ParsecT [Token] st IO (Token)
booleanToken = tokenPrim show update_pos get_token where
  get_token (Boolean x p) = Just (Boolean x p)
  get_token _       = Nothing

charToken :: ParsecT [Token] st IO (Token)
charToken = tokenPrim show update_pos get_token where
  get_token (Char x p) = Just (Char x p)
  get_token _       = Nothing

stringToken :: ParsecT [Token] st IO (Token)
stringToken = tokenPrim show update_pos get_token where
  get_token (String x p) = Just (String x p)
  get_token _       = Nothing

----------------------------------------
--  Auxiliary Functions
----------------------------------------

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- needs improvement
update_pos pos _ []      = pos  

