module Main (main) where

import Tokens
import Text.Parsec
import Control.Monad.IO.Class

import System.IO.Unsafe

----------------------------------------
-- Tokens
----------------------------------------

programToken = tokenPrim show update_pos get_token where
  get_token Program = Just Program
  get_token _       = Nothing

idToken = tokenPrim show update_pos get_token where
  get_token (Id x) = Just (Id x)
  get_token _      = Nothing

beginToken = tokenPrim show update_pos get_token where
  get_token Begin = Just Begin
  get_token _     = Nothing

endToken = tokenPrim show update_pos get_token where
  get_token End = Just End
  get_token _   = Nothing

semiColonToken :: ParsecT [Token] st IO (Token)
semiColonToken = tokenPrim show update_pos get_token where
  get_token SemiColon = Just SemiColon
  get_token _         = Nothing

--colonToken = tokenPrim show update_pos get_token where
--  get_token Colon = Just Colon
--  get_token _     = Nothing

assignToken = tokenPrim show update_pos get_token where
  get_token Assign = Just Assign
  get_token _      = Nothing

constToken :: ParsecT [Token] st IO (Token)
constToken = tokenPrim show update_pos get_token where
  get_token Const   = Just Const
  get_token _       = Nothing

beginExpToken = tokenPrim show update_pos get_token where
  get_token BeginExp = Just BeginExp
  get_token _        = Nothing

endExpToken = tokenPrim show update_pos get_token where
  get_token EndExp = Just BeginExp
  get_token _      = Nothing

intToken = tokenPrim show update_pos get_token where
  get_token (Int x) = Just (Int x)
  get_token _       = Nothing

typeToken = tokenPrim show update_pos get_token where
  get_token (Type x) = Just (Type x)
  get_token _        = Nothing 

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos  

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

-- Const declarations
preDecls :: ParsecT [Token] [(Token,Token)] IO([Token])
preDecls = do
          first <- constDecl <|> varDecl <|> function 
                    <|> voidTokens
          next <- remaining_preDecls <|> voidTokens
          return (first ++ next)

remaining_preDecls :: ParsecT [Token] [(Token,Token)] IO([Token])
remaining_preDecls = (do a <- constDecl <|> varDecl
                         b <- remaining_preDecls
                         return (a ++ b)) <|> (return [])

voidTokens :: ParsecT [Token] [(Token,Token)] IO([Token])
voidTokens =  do
            return []

constDecl :: ParsecT [Token] [(Token,Token)] IO([Token])
constDecl = do
            c <- constToken
            a <- typeToken
            b <- idToken
            e <- semiColonToken
            updateState(symtable_insert (b, get_default_value a))
            s <- getState
            liftIO (print s)
            return (c:a:b:[e])

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
            a <- typeToken
            b <- idToken
            c <- beginExpToken
            d <- endExpToken
            return a:[b]

stmts :: ParsecT [Token] [(Token,Token)] IO([Token])
stmts = do
          first <- assign <|> varDecl
          next <- remaining_stmts
          return (first ++ next)

assign :: ParsecT [Token] [(Token,Token)] IO([Token])
assign = do
          a <- idToken
          b <- assignToken
          c <- intToken
          d <- semiColonToken
          updateState(symtable_update (a, c))
          s <- getState
          liftIO (print s)
          return (a:b:c:[d])

remaining_stmts :: ParsecT [Token] [(Token,Token)] IO([Token])
remaining_stmts = (do a <- assign <|> varDecl
                      b <- remaining_stmts
                      return (a ++ b)) <|> (return [])

-- funções para a tabela de símbolos

get_default_value :: Token -> Token
get_default_value (Type "int") = Int 0          

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
main = case unsafePerformIO (parser (getTokens "../language_examples/programaV1V2.pe")) of
            { Left err -> print err; 
              Right ans -> print ans
            }
