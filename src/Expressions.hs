module Expressions where

import Tokens
import Lexer
import SymTable
import Data.List

import Text.Parsec
import Control.Monad.IO.Class

import System.IO.Unsafe

-- TODO inplicit cohersion on atribution int Token to float var
--

variable :: ParsecT [Token] CapivaraState IO(Token)
variable =  (do 
                s <- getState
                a <- idToken
                return (getVal a (
                    getCurrentScope(s))
                  )
             )
 
getVal :: Token -> Scope -> Token
getVal (Id _ (l, c)) (_,_,[]) = error ("variable not declared in the scope at line " ++ (show l) ++ " column " ++ (show c))
getVal (Id id1 p1) (i,j,((Id id2 _,val)):t) = if id1 == id2 then val
                                         else getVal (Id id1 p1) (i,j,t)

----------------------------------------
-- Expressions
----------------------------------------
-- @TODO Expand expressions
-- <bool_expression> = <term-1>, { [ ( “and” | “or” | “xor” ), <term-1> ] };
-- <term-1> = <expression>, { [ ( “==” | “!=” | “<” | “>” | “<=” | “>=” ), <expression> ] };
-- <expression> = <term-3>, { [ ( “+” | “-” ), <term-3> ] };
-- <term-3> = <term-4>, { [ ( “*” | “/” | “mod” ), <term-4> ] };
-- <term-4> = <factor>, { [ ( “**” ), <factor> ] } | ( “++” | “--” ), <term-1>;
-- <factor> = identifier | <literal> | “(”, <expression>, “)”;
-- <literal> = int-literal | float-literal | boolean-literal | string-literal

--------------------- Level 1 --------------------
expression :: ParsecT [Token] CapivaraState IO(Token)
expression = try bin_expression <|> una_expression

una_expression :: ParsecT [Token] CapivaraState IO(Token)
una_expression =  (do
                      op <- opandToken <|> oporToken <|> opxorToken
                      a <- term1
                      return (a))

--- funções considerando associatividade à esquerda                  
bin_expression :: ParsecT [Token] CapivaraState IO(Token)
bin_expression = do
                   n1 <- term1
                   result <- eval_remaining n1
                   return (result)

eval_remaining :: Token -> ParsecT [Token] CapivaraState IO(Token)
eval_remaining n1 = do
                      op <- opandToken <|> oporToken <|> opxorToken
                      n2 <- term1
                      result <- eval_remaining (eval n1 op n2)
                      return (result) 
                    <|> return (n1)         

--------------------- Level 2 --------------------
term1 :: ParsecT [Token] CapivaraState IO(Token)
term1 = try bin_term1 <|> una_term1

una_term1 :: ParsecT [Token] CapivaraState IO(Token)
una_term1 =  (do
                      op <- equalToken <|> differentToken <|> greaterToken <|> lessToken <|> greaterorequalToken <|> lessorequalToken
                      a <- term2
                      return (a))

--- funções considerando associatividade à esquerda                  
bin_term1 :: ParsecT [Token] CapivaraState IO(Token)
bin_term1 = do
                   n1 <- term2
                   result <- eval_term1 n1
                   return (result)     

eval_term1 :: Token -> ParsecT [Token] CapivaraState IO(Token)
eval_term1 n1 = do
                    op <- equalToken <|> differentToken <|> greaterToken <|> lessToken <|> greaterorequalToken <|> lessorequalToken
                    n2 <- term2
                    result <- eval_term1 (eval n1 op n2)
                    return (result) 
                    <|> return (n1)

--------------------- Level 3 --------------------
term2 :: ParsecT [Token] CapivaraState IO(Token)
term2 = try bin_term2 <|> una_term2

una_term2 :: ParsecT [Token] CapivaraState IO(Token)
una_term2 =  (do
                      op <- plusToken <|> minusToken
                      a <- term3
                      return (a))

--- funções considerando associatividade à esquerda                  
bin_term2 :: ParsecT [Token] CapivaraState IO(Token)
bin_term2 = do
                   n1 <- term3
                   result <- eval_term2 n1
                   return (result)             

eval_term2 :: Token -> ParsecT [Token] CapivaraState IO(Token)
eval_term2 n1 = do
                    op <- plusToken <|> minusToken
                    n2 <- term3
                    result <- eval_term2 (eval n1 op n2)
                    return (result) 
                    <|> return (n1)

--------------------- Level 4 --------------------
term3 :: ParsecT [Token] CapivaraState IO(Token)
term3 = try bin_term3 <|> una_term3

una_term3 :: ParsecT [Token] CapivaraState IO(Token)
una_term3 =  (do
                op <- multToken <|> divToken <|> modToken
                a <- term4
                return (a))
 
--- funções considerando associatividade à esquerda                  
bin_term3 :: ParsecT [Token] CapivaraState IO(Token)
bin_term3 = do
                   n1 <- term4
                   result <- eval_term3 n1
                   return (result)

eval_term3 :: Token -> ParsecT [Token] CapivaraState IO(Token)
eval_term3 n1 = do
                    op <- multToken <|> divToken <|> modToken
                    n2 <- term4
                    result <- eval_term3 (eval n1 op n2)
                    return (result) 
                    <|> return (n1)                              

--------------------- Level 5 --------------------
term4 :: ParsecT [Token] CapivaraState IO(Token)
term4 = try bin_term4 <|> una_term4

una_term4 :: ParsecT [Token] CapivaraState IO(Token)
una_term4 =  (do
                op <- powerToken <|> plusplusToken
                a <- intToken <|> booleanToken <|> floatToken <|> stringToken
                    <|> variable <|> try listLiteral <|> try matrixLiteral 
                    <|> parenExpr
                return (a))
 
--- funções considerando associatividade à esquerda                  
bin_term4 :: ParsecT [Token] CapivaraState IO(Token)
bin_term4 = do
                   n1 <- intToken <|> booleanToken <|> floatToken <|> 
                      stringToken <|> variable <|>  
                      try listLiteral <|> try matrixLiteral <|> parenExpr
                   result <- eval_term4 n1
                   return (result)

eval_term4 :: Token -> ParsecT [Token] CapivaraState IO(Token)
eval_term4 n1 = do
                 op <- powerToken <|> plusplusToken
                 n2 <- intToken <|> booleanToken  <|> floatToken <|> 
                    stringToken <|> variable <|> try listLiteral <|> 
                    try matrixLiteral <|> parenExpr
                 result <- eval_term4 (eval n1 op n2)
                 return (result) 
                 <|> return (n1) 

parenExpr = do
                 a <- beginbracketToken
                 b <- expression
                 c <- endbracketToken
                 return b

listLiteral :: ParsecT [Token] CapivaraState IO(Token)
listLiteral = (do
                a <- beginlistToken
                b <- manyTill (floatToken <|> intToken)
                -- <|> booleanToken  <|> floatToken <|> stringToken) 
                      (endlistToken) <|>
                      manyTill variable (endlistToken) <|> (do return [])
                return (tokens2List b))

matrixLiteral :: ParsecT [Token] CapivaraState IO(Token)
matrixLiteral = (do
                a <- beginlistToken
                b <- manyTill (listLiteral) (endlistToken) <|>
                    manyTill variable (endlistToken)
                return (tokens2Matrix b))

-- makeMatrix :: [Token] -> Token
-- makeMatrix ls = CpvMatrix (map getTokenList (ls))

getTokenList :: Token -> [Float]
getTokenList (CapivaraList tks) = tks

-- Expression evaluation
eval :: Token -> Token -> Token -> Token
eval (Int x p) (Plus _ ) (Int y _) = Int (x + y) p
eval (Float x p) (Plus _ ) (Int y _) = Float (x + (fromIntegral y)) p
eval (Int x p) (Plus _ ) (Float y _) = Float (fromIntegral x + y) p
eval (Float x p) (Plus _ ) (Float y _) = Float (x + y) p

eval (Int x p) (Minus _ ) (Int y _) = Int (x - y) p
eval (Float x p) (Minus _ ) (Int y _) = Float (x - (fromIntegral y)) p
eval (Int x p) (Minus _ ) (Float y _) = Float (fromIntegral x - y) p
eval (Float x p) (Minus _ ) (Float y _) = Float (x - y) p

eval (Int x p) (Mult _ ) (Int y _) = Int (x * y) p
eval (Float x p) (Mult _ ) (Int y _) = Float (x * ( fromIntegral y)) p
eval (Int x p) (Mult _ ) (Float y _) = Float ((fromIntegral x) * y) p
eval (Float x p) (Mult _ ) (Float y _) = Float (x * y) p

eval _ (Div _ ) (Int 0 (l,c)) = error ("Error: Division by zero on line " ++ (show l) ++ " column " ++ (show c))
eval _ (Div _ ) (Float 0 (l,c)) = error ("Error: Division by zero on line " ++ (show l) ++ " column " ++ (show c))

eval (Int x p) (Div _ ) (Int y _) = Int (x `div` y) p
eval (Float x p) (Div _ ) (Int y _) = Float (x / fromIntegral y) p
eval (Int x p) (Div _ ) (Float y _) = Float (fromIntegral x / y) p
eval (Float x p) (Div _ ) (Float y _) = Float (x / y) p

eval _ (Mod _ ) (Int 0 (l,c)) = error ("Error: Division by zero on line " ++ (show l) ++ " column " ++ (show c))
eval _ (Mod _ ) (Float 0 (l,c)) = error ("Error: Division by zero on line " ++ (show l) ++ " column " ++ (show c))

eval (Int x p) (Mod _ ) (Int y _) = Int (x `mod` y) p

eval (Int x p) (Power _ ) (Int y _) = Int (x ^ y) p
eval (Float x p) (Power _ ) (Int y _) = Float (x ** (fromIntegral(y))) p
eval (Int x p) (Power _ ) (Float y _) = Float ((fromIntegral x) ** y) p
eval (Float x p) (Power _ ) (Float y _) = Float (x ** y) p

eval (Boolean x p) (OpAnd _ ) (Boolean y _) = Boolean (x && y) p
eval (Boolean x p) (OpOr _ ) (Boolean y _) = Boolean (x || y) p
eval (Boolean x p) (OpXor _ ) (Boolean y _) = Boolean (x /= y) p

eval (Int x p) (Equal _ ) (Int y _) = Boolean (x == y) p
eval (Float x p) (Equal _ ) (Int y _) = Boolean (x == (fromIntegral y)) p
eval (Int x p) (Equal _ ) (Float y _) = Boolean ((fromIntegral x) == y) p
eval (Float x p) (Equal _ ) (Float y _) = Boolean (x == y) p
eval (Boolean x p) (Equal _ ) (Boolean y _) = Boolean (x == y) p
eval (String x p) (Equal _ ) (String y _) = Boolean (x == y) p
eval (Char x p) (Equal _ ) (Char y _) = Boolean (x == y) p

eval (Int x p) (Different _ ) (Int y _) = Boolean (x /= y) p
eval (Float x p) (Different _ ) (Int y _) = Boolean (x /= (fromIntegral y)) p
eval (Int x p) (Different _ ) (Float y _) = Boolean ((fromIntegral x) /= y) p
eval (Float x p) (Different _ ) (Float y _) = Boolean (x /= y) p
eval (Boolean x p) (Different _ ) (Boolean y _) = Boolean (x /= y) p
eval (String x p) (Different _ ) (String y _) = Boolean (x /= y) p
eval (Char x p) (Different _ ) (Char y _) = Boolean (x /= y) p

eval (Int x p) (Greater _ ) (Int y _) = Boolean (x > y) p
eval (Float x p) (Greater _ ) (Int y _) = Boolean (x > (fromIntegral y)) p
eval (Int x p) (Greater _ ) (Float y _) = Boolean ((fromIntegral x) > y) p
eval (Float x p) (Greater _ ) (Float y _) = Boolean (x > y) p

eval (Int x p) (Less _ ) (Int y _) = Boolean (x < y) p
eval (Float x p) (Less _ ) (Int y _) = Boolean (x < (fromIntegral y)) p
eval (Int x p) (Less _ ) (Float y _) = Boolean ((fromIntegral x) < y) p
eval (Float x p) (Less _ ) (Float y _) = Boolean (x < y) p

eval (Int x p) (GreaterOrEqual _ ) (Int y _) = Boolean (x >= y) p
eval (Float x p) (GreaterOrEqual _ ) (Int y _) = Boolean (x >= (fromIntegral y)) p
eval (Int x p) (GreaterOrEqual _ ) (Float y _) = Boolean ((fromIntegral x) >= y) p
eval (Float x p) (GreaterOrEqual _ ) (Float y _) = Boolean (x >= y) p

eval (Int x p) (LessOrEqual _ ) (Int y _) = Boolean (x <= y) p
eval (Float x p) (LessOrEqual _ ) (Int y _) = Boolean (x <= (fromIntegral y)) p
eval (Int x p) (LessOrEqual _ ) (Float y _) = Boolean ((fromIntegral x) <= y) p
eval (Float x p) (LessOrEqual _ ) (Float y _) = Boolean (x <= y) p

----- Structured Types -----
eval (CapivaraList xs) (Plus p) (CapivaraList ys) = 
   CapivaraList (zipWith (+) xs ys)
eval (CpvMatrix xs) (Plus p) (CpvMatrix ys) =
   (CpvMatrix (zipWith (zipWith (+)) xs ys))

eval (CapivaraList []) (PlusPlus p) (Float x _) = CapivaraList [x]
eval (CapivaraList xs) (PlusPlus p) (Float x _) = CapivaraList (xs ++ [x])
eval (CapivaraList []) (PlusPlus p) (Int x _) = CapivaraList [fromIntegral x]
eval (CapivaraList xs) (PlusPlus p) (Int x _) = CapivaraList (xs ++ [fromIntegral x])

eval (CpvMatrix []) (PlusPlus p) (CapivaraList ys) = CpvMatrix ([ys])
eval (CpvMatrix [[]]) (PlusPlus p) (CapivaraList ys) = CpvMatrix ([ys])
eval (CpvMatrix xs) (PlusPlus p) (CapivaraList ys) = CpvMatrix (xs ++ [ys])
eval (CpvMatrix xs) (Mult p) (CpvMatrix ys) = CpvMatrix (
      [[ sum $ zipWith (*) ar bc | bc <- (transpose ys)] | ar <- xs ]
    )
eval _ _ _ = error("Type error on evaluation")

sumTokens :: [Token] -> Token
sumTokens [] = (Int 0 (0,0))
sumTokens [t] = t
sumTokens (t:ts) = eval t (Mult (0,0)) (sumTokens ts)

tokens2Matrix :: [Token] -> Token
tokens2Matrix [] = CpvMatrix []
tokens2Matrix ((CapivaraList xs):ts) = (CpvMatrix (xs:xss)) where
                      CpvMatrix xss = (tokens2Matrix ts)
tokens2List :: [Token] -> Token
tokens2List [] = CapivaraList []
tokens2List ((Int x p):ts) = CapivaraList ((fromIntegral x):xs) where
                    CapivaraList xs = (tokens2List ts)
tokens2List ((Float x p):ts) = CapivaraList (x:xs) where
                    CapivaraList xs = (tokens2List ts)


