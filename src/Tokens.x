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
  begin                                { \p s -> Begin}
  program                              { \p s -> Program }
  end                                  { \p s -> End}
  const                                { \p s -> Const}
  function                             { \p s -> Function}
  then                                 { \p s -> Then}
  write                                { \p s -> Write}
  "{"                                  { \p s -> BeginScope}
  "}"                                  { \p s -> EndScope}
  "("                                  { \p s -> BeginBracket}
  ")"                                  { \p s -> EndBracket}
  "["                                  { \p s -> BeginList}
  "]"                                  { \p s -> EndList}
  ":"                                  { \p s -> Colon}
  ";"                                  { \p s -> SemiColon}
  ","                                  { \p s -> Comma} 
  int                                  { \p s -> Type s}
  float                                { \p s -> Type s}
  char                                 { \p s -> Type s}
  boolean                              { \p s -> Type s}
  string                               { \p s -> Type s}
  List                                 { \p s -> Type s}
  Mat                                  { \p s -> Type s}
  Table                                { \p s -> Type s}
  "="                                  { \p s -> Assign}
  "=="                                 { \p s -> Equal}
  "!="                                 { \p s -> Different}
  ">"                                  { \p s -> Greater}
  "<"                                  { \p s -> Less}
  ">="                                 { \p s -> GreaterOrEqual}
  "<="                                 { \p s -> LessOrEqual}
  "++"                                 { \p s -> PlusPlus}
  "+"                                  { \p s -> Plus}
  "--"                                 { \p s -> MinusMinus}
  "-"                                  { \p s -> Minus}
  "**"                                 { \p s -> Power}
  "*"                                  { \p s -> Mult}
  "/"                                  { \p s -> Div}
  "mod"                                { \p s -> Mod}
  if                                   { \p s -> If}
  elif                                 { \p s -> Elif}
  else                                 { \p s -> Else}
  switch                               { \p s -> Switch}
  case                                 { \p s -> Case}
  or                                   { \p s -> OpOr}
  xor                                  { \p s -> OpXor}
  and                                  { \p s -> OpAnd}
  $digit+                              { \p s -> Int (read s) }
  $digit+\.$digit+                     { \p s -> Float (read s) }
  \'.\'                                { \p s -> Char (read s) }
  "true"                               { \p s -> Boolean s}
  "false"                              { \p s -> Boolean s}
  $alpha [$alpha $digit \_ \']*        { \p s -> Id s }
  \" $alpha [. [^\"] \']* \"           { \p s -> String s}
{
-- Each action has type :: String -> Token
-- The token type:
data Token =
  Program         (Int, Int) (Int, Int) |
  Begin           (Int, Int) |
  End             (Int, Int) |
  Colon           (Int, Int) |
  SemiColon       (Int, Int) |
  Comma           (Int, Int) |
  Assign          (Int, Int) | 
  Const           (Int, Int) |
  Function        (Int, Int) |
  If              (Int, Int) |
  Then            (Int, Int) |
  Write           (Int, Int) |
  BeginScope      (Int, Int) |
  EndScope        (Int, Int) |
  BeginBracket    (Int, Int) |
  EndBracket      (Int, Int) |
  BeginList       (Int, Int) |
  EndList         (Int, Int) |
  PlusPlus        (Int, Int) |
  Plus            (Int, Int) |
  MinusMinus      (Int, Int) |
  Minus           (Int, Int) |
  Mult            (Int, Int) |
  Div             (Int, Int) |
  Mod             (Int, Int) |
  Power           (Int, Int) |
  Elif            (Int, Int) |
  Else            (Int, Int) |
  Switch          (Int, Int) |
  Case            (Int, Int) |
  OpOr            (Int, Int) |
  OpXor           (Int, Int) |
  OpAnd           (Int, Int) |
  Equal           (Int, Int) |
  Different       (Int, Int) |
  Greater         (Int, Int) |
  Less            (Int, Int) |
  GreaterOrEqual  (Int, Int) |
  LessOrEqual     (Int, Int) |
  Type String     (Int, Int) |
  Id   String     (Int, Int) |
  Int  Int        (Int, Int) |
  Float Float     (Int, Int) |
  Char Char       (Int, Int) |
  Boolean String  (Int, Int) |
  String   String (Int, Int)
  deriving (Eq,Show)

getLC (AlexPn _ l c) = (l, c)  

getTokens fn = unsafePerformIO (getTokensAux fn)
getTokensAux fn = do {fh <- openFile fn ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}
}
