module Main (main) where

import Tokens
import Lexer
import Expressions
import Text.Parsec
import Control.Monad.IO.Class

import System.IO.Unsafe
import System.Environment

-- parsers para os não-terminais

program :: ParsecT [Token] [(Token,Token)] IO ([Token])
program = do
            p <- preDecls
            d <- beginToken 
            a <- programToken 
            e <- stmts
            f <- endToken
            g <- programToken 
            eof
            return (p ++ [d] ++ [a] ++ e ++ f:[g])

-- Pre declarations
preDecls :: ParsecT [Token] [(Token,Token)] IO([Token])
preDecls = (do a <- constDecl <|> varDecl <|> function
               b <- preDecls
               return (a ++ b)) <|> (return [])

voidTokens :: ParsecT [Token] [(Token,Token)] IO([Token])
voidTokens =  do
            return []

constDecl :: ParsecT [Token] [(Token,Token)] IO([Token])
constDecl = do
            z <- constToken
            a <- typeToken
            b <- idToken
            i <- assignToken
            c <- expression
            e <- semiColonToken
            updateState(symtable_insert (b, get_default_value a))
            s <- getState
            liftIO (print s)
            return (z:a:b:i:c ++ [e])

varDecl :: ParsecT [Token] [(Token,Token)] IO([Token])
varDecl = do
            a <- typeToken
            b <- idToken
            e <- semiColonToken
            updateState(symtable_insert (b, get_default_value a))
            s <- getState
            liftIO (print s)
            return (a:b:[e])

function :: ParsecT [Token] [(Token,Token)] IO([Token])
function = do
            f <- functionToken
            a <- typeToken
            b <- idToken
            c <- beginBracketToken
--            p <- paramsList
            d <- endBracketToken
            e <- semiColonToken
            return (f:a:b:[c] ++ d:[e])

--paramsList :: ParsecT [Token] [(Token,Token)] IO([Token])
--paramsList = (do
--               a <- typeToken
--               b <- idToken
--               return (a:[b])) <|>
--             (do
--               c <- commaToken
--               a <- typeToken
--               b <- idToken
--               c <- paramsList
--               return (a:b:c)) <|> (return [])
               --b <- colonToken
               --c <- identifiersList
               --d <- semiColonToken
               --e <- paramsList
               --return (a:b:c ++ d:e)) <|> (return [])

identifiersList = (do
                    a <- idToken
                    b <- commaToken
                    c <- identifiersList
                    return (a:b:c)) <|> (return [])

stmts :: ParsecT [Token] [(Token,Token)] IO([Token])
stmts = (do a <- assign <|> varDecl
            b <- stmts
            return (a ++ b)) <|> (return [])

assign :: ParsecT [Token] [(Token,Token)] IO([Token])
assign = do
          a <- idToken
          b <- assignToken
          c <- intToken <|> floatToken <|> booleanToken <|> charToken 
                <|> stringToken
          d <- semiColonToken
          updateState(symtable_update (a, c))
          s <- getState
          liftIO (print s)
          return (a:b:c:[d])

-- funções para a tabela de símbolos

get_default_value :: Token -> Token
get_default_value (Type "int") = Int 0
get_default_value (Type "float") = Float 0.0
get_default_value (Type "boolean") = Boolean "false"
get_default_value (Type "char") = Char 'a'
get_default_value (Type "string") = String ""

symtable_insert :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_insert symbol []  = [symbol]
symtable_insert symbol symtable = symtable ++ [symbol]

symtable_update :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_update _ [] = fail "variable not found"
symtable_update (id1, v1) ((id2, v2):t) = 
                               if id1 == id2 then (id1, v1) : t
                               else (id2, v2) : symtable_update (id1, v1) t

symtable_remove :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_remove _ [] = fail "variable not found"
symtable_remove (id1, v1) ((id2, v2):t) = 
                               if id1 == id2 then t
                               else (id2, v2) : symtable_remove (id1, v1) t                               


-- invocação do parser para o símbolo de partida 

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program [] "Error message" tokens

main :: IO ()
main = do
       (file:args) <-getArgs
       putStrLn file
       case unsafePerformIO (parser (getTokens file)) of
            { Left err -> print err; 
              Right ans -> print ans
            }
