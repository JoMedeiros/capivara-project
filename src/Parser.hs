-- module Main (main) where
module Parser where

import Tokens
import Lexer
import Expressions
import Text.Parsec
import Control.Monad.IO.Class
import SymTable

-- import System.IO.Unsafe
-- import System.Environment

-- EBNF based:

program :: ParsecT [Token] [(MemCell)] IO ([Token])
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
preDecls :: ParsecT [Token] [(MemCell)] IO([Token])
preDecls = (do a <- constDecl <|> varDecl <|> function <|> procedure
               b <- preDecls
               return (a ++ b)) <|> (return [])

voidTokens :: ParsecT [Token] [(MemCell)] IO([Token])
voidTokens =  do
            return []

constDecl :: ParsecT [Token] [(MemCell)] IO([Token])
constDecl = do
            z <- constToken
            a <- typeToken
            b <- idToken
            i <- assignToken
            c <- expression
            e <- semicolonToken
            updateState(symtable_insert (Var (b, c)))
            s <- getState
            liftIO (print s)
            return (z:a:b:i:c:[e])

varDecl :: ParsecT [Token] [(MemCell)] IO([Token])
varDecl = do
            a <- typeToken
            b <- idToken
            e <- semicolonToken
            updateState(symtable_insert (Var (b, get_default_value a)))
            s <- getState
            liftIO (print s)
            return (a:b:[e])

function :: ParsecT [Token] [(MemCell)] IO([Token])
function = do
            f <- functionToken
            a <- typeToken
            b <- idToken
            c <- beginbracketToken
            p <- paramsList
            d <- endbracketToken
            e <- block
            return (f:a:b:[c] ++ d:e)

procedure :: ParsecT [Token] [(MemCell)] IO([Token])
procedure = do
            f <- procedureToken
            b <- idToken
            c <- beginbracketToken
            p <- paramsList
            d <- endbracketToken
            e <- block
            return (f:b:c:p ++ (d:e))

paramsList :: ParsecT [Token] [(MemCell)] IO([Token])
paramsList = (do
               a <- typeToken
               b <- idToken
               c <- paramsList
               return (a:b:c)) <|>
             (do
               a <- commaToken
               b <- typeToken
               c <- idToken
               d <- paramsList
               return (a:b:c:d)) <|> (return [])

identifiersList = (do
                    a <- idToken
                    b <- identifiersList
                    return (a:b)) <|>
                  (do
                    a <- idToken
                    b <- commaToken
                    c <- identifiersList
                    return (a:b:c)) <|> (return [])

stmts :: ParsecT [Token] [(MemCell)] IO([Token])
stmts = (do a <- assign <|> varDecl <|> block
            b <- stmts
            return (a ++ b)) <|> (return [])

block :: ParsecT [Token] [(MemCell)] IO([Token])
block = (do a <- beginscopeToken
            b <- stmts
            c <- endscopeToken
            return (a:b ++ [c]))

assign :: ParsecT [Token] [(MemCell)] IO([Token])
assign = do
          a <- idToken
          b <- assignToken
          c <- expression 
          --intToken <|> floatToken <|> booleanToken <|> charToken 
            --    <|> stringToken
          d <- semicolonToken
          updateState(symtable_update (Var (a, c)))
          s <- getState
          liftIO (print s)
          return (a:b:c:[d])

-- invocação do parser para o símbolo de partida 

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program [] "Error message" tokens

-- main :: IO ()
-- main = do
--        (file:args) <-getArgs
--        putStrLn file
--        case unsafePerformIO (parser (getTokens file)) of
--             { Left err -> print err; 
--               Right ans -> print ans
--             }
