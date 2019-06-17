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


