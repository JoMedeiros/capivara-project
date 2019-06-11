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

program :: ParsecT [Token] CapivaraState IO ([Token])
program = do
            p <- preDecls
            s <- getState
            liftIO (print s)
            d <- beginToken 
            a <- programToken 
            updateState( initScope )
            e <- stmts
            f <- endToken
            g <- programToken 
            eof
            return (p ++ [d] ++ [a] ++ e ++ f:[g])

-- Pre declarations
preDecls :: ParsecT [Token] CapivaraState IO([Token])
preDecls = (do a <- constDecl <|> varDecl <|> function <|> procedure
               b <- preDecls
               return (a ++ b)) <|> (return [])

voidTokens :: ParsecT [Token] CapivaraState IO([Token])
voidTokens =  do
            return []

constDecl :: ParsecT [Token] CapivaraState IO([Token])
constDecl = do
            z <- constToken
            a <- typeToken
            b <- idToken
            i <- assignToken
            c <- expression
            e <- semicolonToken
            updateState(capivaraStateInsert ( b, c ))
            s <- getState
            liftIO (print s)
            return (z:a:b:i:c:[e])

varDecl :: ParsecT [Token] CapivaraState IO([Token])
varDecl = do
            a <- typeToken
            b <- idToken
            e <- semicolonToken
            updateState(capivaraStateInsert ( (b, get_default_value a)))
            s <- getState
            liftIO (print s)
            return (a:b:[e])

ifStatement :: ParsecT [Token] CapivaraState IO([Token])
ifStatement = do
            f <- ifToken
            a <- beginbracketToken
            b <- expression
            c <- endbracketToken
            d <- block
            return (f:a:b:c:d)

whileStatement :: ParsecT [Token] CapivaraState IO([Token])
whileStatement = do
            f <- whileToken
            a <- beginbracketToken
            b <- expression
            c <- endbracketToken
            d <- block
            return (f:a:b:c:d)
{-
forStatement :: ParsecT [Token] CapivaraState IO([Token])
forStatement = do
            f <- forToken
            a <- beginbracketToken
            
            b <- 

            c <- semicolonToken
            d <- expression
            e <- semicolonToken
            g <- expression
            h <- endbracketToken
            i <- expression
            j <- block
            return (f:a:b:c:d:e:g:h:i:j)            
-}
function :: ParsecT [Token] CapivaraState IO([Token])
function = do
            f <- functionToken
            a <- typeToken
            b <- idToken
            c <- beginbracketToken
            p <- paramsList
            d <- endbracketToken
            e <- block
            return (f:a:b:c:d:e)

procedure :: ParsecT [Token] CapivaraState IO([Token])
procedure = do
            f <- procedureToken
            b <- idToken
            c <- beginbracketToken
            p <- paramsList
            d <- endbracketToken
            e <- block
            return (f:b:c:p ++ (d:e))

paramsList :: ParsecT [Token] CapivaraState IO([Token])
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
                    b <- commaToken
                    c <- identifiersList
                    return (a:b:c)) <|>
                  (do
                    a <- idToken
                    b <- identifiersList
                    return (a:b)) <|> (return [])

stmts :: ParsecT [Token] CapivaraState IO([Token])
stmts = (do a <- assign <|> varDecl <|> block
            b <- stmts
            return (a ++ b)) <|> (return [])

block :: ParsecT [Token] CapivaraState IO([Token])
block = (do a <- beginscopeToken
            b <- stmts
            c <- endscopeToken
            return (a:b ++ [c]))

assign :: ParsecT [Token] CapivaraState IO([Token])
assign = do
          a <- idToken
          b <- assignToken
          c <- expression 
          --intToken <|> floatToken <|> booleanToken <|> charToken 
            --    <|> stringToken
          d <- semicolonToken
          updateState(capivaraStateUpdate ( (a, c)))
          s <- getState
          liftIO (print s)
          return (a:b:c:[d])

-- invocação do parser para o símbolo de partida 

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program (0,0,[],[]) "Error message" tokens

-- main :: IO ()
-- main = do
--        (file:args) <-getArgs
--        putStrLn file
--        case unsafePerformIO (parser (getTokens file)) of
--             { Left err -> print err; 
--               Right ans -> print ans
--             }
