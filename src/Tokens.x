{
module Tokens where

import System.IO
import System.IO.Unsafe
}

%wrapper "posn"

$digit = 0-9      -- digits
$alpha = [a-zA-Z] -- alphabetic characters

tokens :-

  $white+                         ;
  "#".*                           ; -- comment
  begin                           { \p s -> Begin (getLC p) }
  program                         { \p s -> Program  (getLC p) }
  end                             { \p s -> End (getLC p) }
  const                           { \p s -> Const (getLC p) }
  function                        { \p s -> Function (getLC p) }
  procedure                       { \p s -> Procedure (getLC p) }
  then                            { \p s -> Then (getLC p) }
  write                           { \p s -> Write (getLC p) }
  "read"                          { \p s -> Read (getLC p) }
  "{"                             { \p s -> BeginScope (getLC p) }
  "}"                             { \p s -> EndScope (getLC p) }
  "("                             { \p s -> BeginBracket (getLC p) }
  ")"                             { \p s -> EndBracket (getLC p) }
  "["                             { \p s -> BeginList (getLC p) }
  "]"                             { \p s -> EndList (getLC p) }
  ":"                             { \p s -> Colon (getLC p) }
  ";"                             { \p s -> SemiColon (getLC p) }
  ","                             { \p s -> Comma  (getLC p) }
  int                             { \p s -> Type s (getLC p) }
  float                           { \p s -> Type s (getLC p) }
  char                            { \p s -> Type s (getLC p) }
  boolean                         { \p s -> Type s (getLC p) }
  string                          { \p s -> Type s (getLC p) }
  List                            { \p s -> Type s (getLC p) }
  Matrix                          { \p s -> Type s (getLC p) }
  Table                           { \p s -> Type s (getLC p) }
  "=="                            { \p s -> Equal (getLC p) }
  "="                             { \p s -> Assign (getLC p) }
  "!="                            { \p s -> Different (getLC p) }
  ">"                             { \p s -> Greater (getLC p) }
  "<"                             { \p s -> Less (getLC p) }
  ">="                            { \p s -> GreaterOrEqual (getLC p) }
  "<="                            { \p s -> LessOrEqual (getLC p) }
  "++"                            { \p s -> PlusPlus (getLC p) }
  "+"                             { \p s -> Plus (getLC p) }
  "--"                            { \p s -> MinusMinus (getLC p) }
  "-"                             { \p s -> Minus (getLC p) }
  "**"                            { \p s -> Power (getLC p) }
  "*"                             { \p s -> Mult (getLC p) }
  "/"                             { \p s -> Div (getLC p) }
  "mod"                           { \p s -> Mod (getLC p) }
  if                              { \p s -> If (getLC p) }
  elif                            { \p s -> Elif (getLC p) }
  while                           { \p s -> While (getLC p) }
  for                             { \p s -> For (getLC p) }
  else                            { \p s -> Else (getLC p) }
  switch                          { \p s -> Switch (getLC p) }
  case                            { \p s -> Case (getLC p) }
  or                              { \p s -> OpOr (getLC p) }
  xor                             { \p s -> OpXor (getLC p) }
  and                             { \p s -> OpAnd (getLC p) }
  $digit+                         { \p s -> Int (read s)  (getLC p) }
  $digit+\.$digit+                { \p s -> Float (read s)  (getLC p) }
  \'.\'                           { \p s -> Char (read s)  (getLC p) }
  "True"                          { \p s -> Boolean (read s) (getLC p) }
  "False"                         { \p s -> Boolean (read s) (getLC p) }
  $alpha [$alpha $digit \_ \']*   { \p s -> Id s  (getLC p) }
  \".*\"                          { \p s -> String s (getLC p) }
{
-- Each action has type :: String -> Token
-- The token type:
data Token =
  Program         (Int, Int) |
  Begin           (Int, Int) |
  End             (Int, Int) |
  Colon           (Int, Int) |
  SemiColon       (Int, Int) |
  Comma           (Int, Int) |
  Assign          (Int, Int) | 
  Const           (Int, Int) |
  Function        (Int, Int) |
  Procedure       (Int, Int) |
  If              (Int, Int) |
  While           (Int, Int) |
  For             (Int, Int) |
  Then            (Int, Int) |
  Write           (Int, Int) |
  Read            (Int, Int) |
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
  Boolean Bool    (Int, Int) |
  String   String (Int, Int) |
  CapivaraList [Float]       |
  CpvMatrix [[Float]]
  deriving (Eq,Show)

getLC (AlexPn _ l c) = (l, c)  

getTokens fn = unsafePerformIO (getTokensAux fn)
getTokensAux fn = do {fh <- openFile fn ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}
}
