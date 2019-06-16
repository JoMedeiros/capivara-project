-- module Main (main) where
module Parser where

import Tokens
import Lexer
import Expressions
import Text.Parsec
import Control.Monad.IO.Class
import SymTable
import qualified StatementParser as LittleBoy

-- import System.IO.Unsafe
-- import System.Environment

-- EBNF based:

program :: ParsecT [Token] CapivaraState IO ([Token])
program = do
            p <- preDecls
            s <- getState
            ----liftIO (print s)
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
preDecls = (do a <- constDecl <|> varDecl <|> function <|> 
                  procedure
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
            --liftIO (print s)
            return (z:a:b:i:c:[e])

varDecl :: ParsecT [Token] CapivaraState IO([Token])
varDecl = do
            a <- typeToken <|> listToken <|> matrixToken
            b <- idToken
            e <- semicolonToken
            updateState(capivaraStateInsert ( (b, get_default_value a)))
            s <- getState
            --liftIO (print s)
            return (a:b:[e])

ifStatement :: ParsecT [Token] CapivaraState IO([Token])
ifStatement = do
            f <- ifToken
            a <- beginbracketToken
            b <- expression
            c <- endbracketToken 
            if (tokenIsTrue b) then ( do
                d <- block
                return (f:a:b:c:d))
            else ( do
              d <- LittleBoy.block
              return (f:a:b:c:d))

-------------------- While --------------------
whileStatement :: ParsecT [Token] CapivaraState IO([Token])
whileStatement = do
            ws <- getInput 
            f <- whileToken
            a <- beginbracketToken
            b <- expression
            c <- endbracketToken
            d <- beginscopeToken
            if (tokenIsTrue b) then ( do
                e <- stmts
                setInput ws
                return (f:a:b:c:d:e))
            else ( do
              e <- LittleBoy.stmts
              f <- endscopeToken
              return (f:a:b:c:d:e))
-- TODO in nested while ignoreBlk stops at first "}"
ignoreBlk :: ParsecT [Token] CapivaraState IO([Token])
ignoreBlk = (do
               a <- manyTill anyToken (endscopeToken)
               return (a)) <|> (return [])

tokenIsTrue :: Token -> Bool
tokenIsTrue (Boolean True _) = True
tokenIsTrue _ = False
-------------------- /While --------------------

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
stmts = (do a <- assign <|> varDecl <|> block <|> capivaraWrite <|> capivaraRead <|> whileStatement <|> ifStatement
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
          d <- semicolonToken
          (_, _, [(_, _, table)], _) <- getState
          -- s <- getState
          if (not (compatible (get_type a table) c)) then 
            error "Type mismatch"
          else
            do
              updateState(capivaraStateUpdate(a, c))
              s <- getState
              --liftIO (print s)
              return (a:b:c:[d])

-- TODO fazer funções para "printar" cada tipo
capivaraWrite :: ParsecT [Token] CapivaraState IO([Token])
capivaraWrite = do
                a <- writeToken
                b <- expression
                c <- semicolonToken
                liftIO (putStr (token2str b))
                return (a:b:[c])

token2str :: Token -> String
token2str (Int v p) = show v
token2str (Float v p) = show v
token2str (Char v p) = show v
token2str (Boolean v p) = show v
token2str (String v p) = read v
token2str (CapivaraList []) = "|"
token2str (CapivaraList (t:ts)) = "| " ++ (show t) ++ " " ++ (token2str (CapivaraList ts))
token2str (CpvMatrix []) = ""
token2str (CpvMatrix (t:ts)) = (token2str (CapivaraList t)) ++ "\n" ++ (token2str (CpvMatrix ts))
token2str t = show t

capivaraRead :: ParsecT [Token] CapivaraState IO([Token])
capivaraRead = do 
               a <- readToken
               b <- greaterToken
               c <- idToken
               d <- semicolonToken
               v <- liftIO $ getLine
               (_, _, [(_, _, table)], _) <- getState
               updateState(capivaraStateUpdate(c, (generateToken (get_type c table) (show v) ))) 
               return ([a])

generateToken :: Token -> String -> Token
generateToken (String _ p ) s = (String s p)
generateToken (Int _ p) s = (Int (read (read s)) p)
generateToken (Float _ p) s = (Float (read (read s)) p)
generateToken (Char _ p) s = (Char (read (read s)) p)
generateToken (Boolean _ p) s = (Boolean (read (read s)) p)
generateToken _ _ = error "error: input type error"

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
