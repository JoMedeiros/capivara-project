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

tokenize :: Token -> ParsecT [Token] st IO (Token)
tokenize token = tokenPrim show update_pos get_token where
  get_token token = Just token
  get_token _     = Nothing

programToken :: ParsecT [Token] st IO (Token)
programToken = tokenize Program

beginToken :: ParsecT [Token] st IO (Token)
beginToken = tokenize Begin

endToken :: ParsecT [Token] st IO (Token)
endToken = tokenize End

colonToken :: ParsecT [Token] st IO (Token)
colonToken = tokenize Colon

semiColonToken :: ParsecT [Token] st IO (Token)
semiColonToken = tokenize SemiColon

assignToken :: ParsecT [Token] st IO (Token)
assignToken = tokenize Assign

constToken :: ParsecT [Token] st IO (Token)
constToken = tokenPrim show update_pos get_token where
  get_token Const   = Just Const
  get_token _       = Nothing

functionToken :: ParsecT [Token] st IO (Token)
functionToken = tokenPrim show update_pos get_token where
  get_token Function   = Just Function
  get_token _       = Nothing

ifToken :: ParsecT [Token] st IO (Token)
ifToken = tokenize If

thenToken :: ParsecT [Token] st IO (Token)
thenToken = tokenize Then

writeToken :: ParsecT [Token] st IO (Token)
writeToken = tokenize Write

beginScopeToken :: ParsecT [Token] st IO (Token)
beginScopeToken = tokenize BeginScope

endScopeToken :: ParsecT [Token] st IO (Token)
endScopeToken = tokenize EndScope

beginBracketToken :: ParsecT [Token] st IO (Token)
beginBracketToken = tokenize BeginBracket

endBracketToken :: ParsecT [Token] st IO (Token)
endBracketToken = tokenize EndBracket

beginListToken :: ParsecT [Token] st IO (Token)
beginListToken = tokenize BeginList

endlistToken :: ParsecT [Token] st IO (Token)
endlistToken = tokenize EndList

plusplusToken :: ParsecT [Token] st IO (Token)
plusplusToken = tokenize PlusPlus

plusToken :: ParsecT [Token] st IO (Token)
plusToken = tokenize Plus

minusMinusToken :: ParsecT [Token] st IO (Token)
minusMinusToken = tokenize MinusMinus

minusToken :: ParsecT [Token] st IO (Token)
minusToken = tokenize Minus

multToken :: ParsecT [Token] st IO (Token)
multToken = tokenize Mult

divToken :: ParsecT [Token] st IO (Token)
divToken = tokenize Div

modToken :: ParsecT [Token] st IO (Token)
modToken = tokenize Mod

powerToken :: ParsecT [Token] st IO (Token)
powerToken = tokenize Power

elifToken :: ParsecT [Token] st IO (Token)
elifToken = tokenize Elif

elseToken :: ParsecT [Token] st IO (Token)
elseToken = tokenize Else

switchToken :: ParsecT [Token] st IO (Token)
switchToken = tokenize Switch

caseToken :: ParsecT [Token] st IO (Token)
caseToken = tokenize Case

oporToken :: ParsecT [Token] st IO (Token)
oporToken = tokenize OpOr

opxorToken :: ParsecT [Token] st IO (Token)
opxorToken = tokenize OpXor

opandToken :: ParsecT [Token] st IO (Token)
opandToken = tokenize OpAnd

equalToken :: ParsecT [Token] st IO (Token)
equalToken = tokenize Equal

differentToken :: ParsecT [Token] st IO (Token)
differentToken = tokenize Different

greaterToken :: ParsecT [Token] st IO (Token)
greaterToken = tokenize Greater

lessToken :: ParsecT [Token] st IO (Token)
lessToken = tokenize Less

greaterorequalToken :: ParsecT [Token] st IO (Token)
greaterorequalToken = tokenize GreaterOrEqual

lessOrEqualToken :: ParsecT [Token] st IO (Token)
lessOrEqualToken = tokenize LessOrEqual

typeToken :: ParsecT [Token] st IO (Token)
typeToken = tokenPrim show update_pos get_token where
  get_token (Type x) = Just (Type x)
  get_token _        = Nothing 

idToken :: ParsecT [Token] st IO (Token)
idToken = tokenPrim show update_pos get_token where
  get_token (Id x) = Just (Id x)
  get_token _      = Nothing

intToken :: ParsecT [Token] st IO (Token)
intToken = tokenPrim show update_pos get_token where
  get_token (Int x) = Just (Int x)
  get_token _       = Nothing

floatToken :: ParsecT [Token] st IO (Token)
floatToken = tokenPrim show update_pos get_token where
  get_token (Float x) = Just (Float x)
  get_token _       = Nothing

booleanToken :: ParsecT [Token] st IO (Token)
booleanToken = tokenPrim show update_pos get_token where
  get_token (Boolean x) = Just (Boolean x)
  get_token _       = Nothing

charToken :: ParsecT [Token] st IO (Token)
charToken = tokenPrim show update_pos get_token where
  get_token (Char x) = Just (Char x)
  get_token _       = Nothing

stringToken :: ParsecT [Token] st IO (Token)
stringToken = tokenPrim show update_pos get_token where
  get_token (String x) = Just (String x)
  get_token _       = Nothing

----------------------------------------
--  Auxiliary Functions
----------------------------------------

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- needs improvement
update_pos pos _ []      = pos  

