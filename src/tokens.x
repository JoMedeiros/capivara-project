{
module Main (main) where
}

%wrapper "basic"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-

  $white+                              ;
  "--".*                               ;
  "{"                                  { \s -> BeginScope}
  "}"                                  { \s -> EndScope}
  "("                                  { \s -> BeginExp}
  ")"                                  { \s -> EndExp}
  "["                                  { \s -> BeginList}
  "]"                                  { \s -> EndList}
  ":"                                  { \s -> Colon}
  ";"                                  { \s -> SemiColon}
  int                                  { \s -> Type s}
  float                                { \s -> Type s}
  char                                 { \s -> Type s}
  boolean                              { \s -> Type s}
  string                               { \s -> Type s}
  List                                 { \s -> Type s}
  Mat                                  { \s -> Type s}
  Table                                { \s -> Type s}
  "="                                  { \s -> Assign}
  ">"                                  { \s -> Greater}
  "<"                                  { \s -> Less}
  "+"                                  { \s -> Plus}
  "-"                                  { \s -> Minus}
  "*"                                  { \s -> Mult}
  "/"                                  { \s -> Div}
  if                                   { \s -> If}
  elif                                 { \s -> Elif}
  else                                 { \s -> Else}
  switch                               { \s -> Switch}
  case                                 { \s -> Case}
  or                                   { \s -> OpOr}
  and                                  { \s -> OpAnd}
  $digit+                              { \s -> Int (read s) }
  $alpha [$alpha $digit \_ \']*        { \s -> Id s }
  \" $alpha [. [^\"] \']* \"  { \s -> String s}

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
  BeginScope|
  EndScope|
  BeginExp|
  EndExp|
  BeginList|
  EndList|
  Colon   |
  SemiColon |
  Assign    |
  Plus|
  Minus|
  Mult|
  Div|
  If    |
  Elif  |
  Else  |
  Switch|
  Case  |
  OpOr  |
  OpAnd |
  Greater |
  Less  |
  Type String|
  Id String |
  Int Int |
  String String
  deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
