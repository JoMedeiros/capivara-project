{
module Tokens where

import System.IO
import System.IO.Unsafe
}

%wrapper "basic"

$digit = 0-9      -- digits
$alpha = [a-zA-Z] -- alphabetic characters

tokens :-

  $white+                              ;
  "--".*                               ; -- comment
  begin                                { \s -> Begin}
  program                              { \s -> Program }
  end                                  { \s -> End}
  const                                { \s -> Const}
  function                             { \s -> Function}
  then                                 { \s -> Then}
  write                                { \s -> Write}
  "{"                                  { \s -> BeginScope}
  "}"                                  { \s -> EndScope}
  "("                                  { \s -> BeginBracket}
  ")"                                  { \s -> EndBracket}
  "["                                  { \s -> BeginList}
  "]"                                  { \s -> EndList}
  ":"                                  { \s -> Colon}
  ";"                                  { \s -> SemiColon}
  ","                                  { \s -> Comma} 
  int                                  { \s -> Type s}
  float                                { \s -> Type s}
  char                                 { \s -> Type s}
  boolean                              { \s -> Type s}
  string                               { \s -> Type s}
  List                                 { \s -> Type s}
  Mat                                  { \s -> Type s}
  Table                                { \s -> Type s}
  "="                                  { \s -> Assign}
  "=="                                 { \s -> Equal}
  "!="                                 { \s -> Different}
  ">"                                  { \s -> Greater}
  "<"                                  { \s -> Less}
  ">="                                 { \s -> GreaterOrEqual}
  "<="                                 { \s -> LessOrEqual}
  "++"                                 { \s -> PlusPlus}
  "+"                                  { \s -> Plus}
  "--"                                 { \s -> MinusMinus}
  "-"                                  { \s -> Minus}
  "**"                                 { \s -> Power}
  "*"                                  { \s -> Mult}
  "/"                                  { \s -> Div}
  "mod"                                { \s -> Mod}
  if                                   { \s -> If}
  elif                                 { \s -> Elif}
  else                                 { \s -> Else}
  switch                               { \s -> Switch}
  case                                 { \s -> Case}
  or                                   { \s -> OpOr}
  xor                                  { \s -> OpXor}
  and                                  { \s -> OpAnd}
  $digit+                              { \s -> Int (read s) }
  $digit+\.$digit                      { \s -> Float (read s) }
  \'.\'                                { \s -> Char (read s) }
  "true"                               { \s -> Boolean s}
  "false"                              { \s -> Boolean s}
  $alpha [$alpha $digit \_ \']*        { \s -> Id s }
  \" $alpha [. [^\"] \']* \"           { \s -> String s}
{
-- Each action has type :: String -> Token
-- The token type:
data Token =
  Program         |
  Begin           |
  End             |
  Colon           |
  SemiColon       |
  Comma           |
  Assign          | 
  Const           |
  Function        |
  If              |
  Then            |
  Write           |
  BeginScope      |
  EndScope        |
  BeginBracket    |
  EndBracket      |
  BeginList       |
  EndList         |
  PlusPlus        |
  Plus            |
  MinusMinus      |
  Minus           |
  Mult            |
  Div             |
  Mod             |
  Power           |
  Elif            |
  Else            |
  Switch          |
  Case            |
  OpOr            |
  OpXor           |
  OpAnd           |
  Equal           |
  Different       |
  Greater         |
  Less            |
  GreaterOrEqual  |
  LessOrEqual     |
  Type String     |
  Id   String     |
  Int  Int        |
  Float Float     |
  Char Char       |
  Boolean String  |
  String   String
  deriving (Eq,Show)

getTokens fn = unsafePerformIO (getTokensAux fn)
getTokensAux fn = do {fh <- openFile fn ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}
}
