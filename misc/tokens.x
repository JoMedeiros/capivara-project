--{
--  module Main (main, Token(..), AlexPosn(..), alexScanTokens, token_posn) where
--}
{
module Tokens where

import System.IO
import System.IO.Unsafe
}

%wrapper "posn"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-

  $white+                              ;
  "--".*                               ; -- comment
  "{"                                  { \p s -> BeginScope}
  "}"                                  { \p s -> EndScope}
  "("                                  { \p s -> BeginExp}
  ")"                                  { \p s -> EndExp}
  "["                                  { \p s -> BeginList}
  "]"                                  { \p s -> EndList}
  ":"                                  { \p s -> Colon}
  ";"                                  { \p s -> SemiColon}
  const                                { \p s -> Const}
  int                                  { \p s -> Type s}
  float                                { \p s -> Type s}
  char                                 { \p s -> Type s}
  boolean                              { \p s -> Type s}
  string                               { \p s -> Type s}
  List                                 { \p s -> Type s}
  Mat                                  { \p s -> Type s}
  Table                                { \p s -> Type s}
  "="                                  { \p s -> Assign}
  ">"                                  { \p s -> Greater}
  "<"                                  { \p s -> Less}
  "+"                                  { \p s -> Plus}
  "-"                                  { \p s -> Minus}
  "*"                                  { \p s -> Mult}
  "/"                                  { \p s -> Div}
  if                                   { \p s -> If}
  elif                                 { \p s -> Elif}
  else                                 { \p s -> Else}
  switch                               { \p s -> Switch}
  case                                 { \p s -> Case}
  or                                   { \p s -> OpOr}
  and                                  { \p s -> OpAnd}
  $digit+                              { \p s -> Int (read s) }
  $alpha [$alpha $digit \_ \']*        { \p s -> Id s }
  \" $alpha [. [^\"] \']* \"           { \p s -> String s}

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
  BeginScope AlexPosn	|
  EndScope   AlexPosn	|
  BeginExp   AlexPosn	|
  EndExp     AlexPosn	|
  BeginList  AlexPosn	|
  EndList    AlexPosn	|
  Colon      AlexPosn	|
  SemiColon  AlexPosn	|
  Assign     AlexPosn	|
  Plus       AlexPosn	|
  Minus      AlexPosn	|
  Mult       AlexPosn	|
  Div        AlexPosn	|
  If         AlexPosn	|
  Elif       AlexPosn	|
  Else       AlexPosn	|
  Switch     AlexPosn	|
  Case       AlexPosn	|
  OpOr       AlexPosn	|
  OpAnd      AlexPosn	|
  Greater    AlexPosn	|
  Const      AlexPosn |
  Less       AlexPosn	|
  Type       AlexPosn	String|
  Id         AlexPosn String|
  Int        AlexPosn	Int|
  String     AlexPosn String
  deriving (Eq,Show)

token_posn (BeginScope p) = p
token_posn (EndScope   p) = p
token_posn (BeginExp   p) = p
token_posn (EndExp     p) = p
token_posn (BeginList  p) = p
token_posn (EndList    p) = p
token_posn (Colon      p) = p
token_posn (SemiColon  p) = p
token_posn (Assign     p) = p
token_posn (Plus       p) = p
token_posn (Minus      p) = p
token_posn (Mult       p) = p
token_posn (Div        p) = p
token_posn (If         p) = p
token_posn (Elif       p) = p
token_posn (Else       p) = p
token_posn (Switch     p) = p
token_posn (Case       p) = p
token_posn (OpOr       p) = p
token_posn (OpAnd      p) = p
token_posn (Greater    p) = p
token_posn (Less       p) = p
token_posn (Type       p _) = p
token_posn (Id         p _) = p
token_posn (Int        p _) = p
token_posn (String     p _) = p

--main = do
--  s <- getContents
--  print (alexScanTokens s)
--}
