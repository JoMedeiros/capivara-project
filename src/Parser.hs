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
            updateState( initScope )
            p <- preDecls
            s <- getState
            ----liftIO (print s)
            d <- beginToken 
            a <- programToken 
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
            return (z:a:b:i:c:[e])

varDecl :: ParsecT [Token] CapivaraState IO([Token])
varDecl = do
            a <- typeToken <|> listToken <|> matrixToken
            b <- idToken
            e <- semicolonToken
            s <- getState
            if ((isInScope b (
                getCurrentScope(s)))) then(do
              liftIO $ error "Variable declared previously.\n"
              return [])
            else ( do
              updateState(capivaraStateInsert ( (b, get_default_value a)))
              return (a:b:[e]))

ifStatement :: ParsecT [Token] CapivaraState IO([Token])
ifStatement = do
            f <- ifToken
            a <- beginbracketToken
            b <- expression
            c <- endbracketToken 
            if (tokenIsTrue b) then ( do
                d <- block
                try (do 
                  e <- elseToken
                  g <- LittleBoy.block
                  return (f:a:b:c:d ++ e:g)) <|> (do return (f:a:b:c:d))
                return (f:a:b:c:d))
            else ( do
              d <- LittleBoy.block
              try (do 
                e <- elseToken
                g <- block
                return (f:a:b:c:d ++ e:g)) <|> (do return (f:a:b:c:d))
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
            e <- LittleBoy.block
            updateState(capivaraStateInsert ( b, CpvFunc e ))
            return (f:a:b:c:d:e)

returnExp :: ParsecT [Token] CapivaraState IO([Token])
returnExp = do
            a <- returnToken
            b <- expression
            c <- semicolonToken
            return (a:b:[c])

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

stmts :: ParsecT [Token] CapivaraState IO([Token])
stmts = (do a <- assign <|> block <|> capivaraWrite <|> 
              capivaraRead <|> whileStatement <|> ifStatement
            -- <|> varDecl
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

--------------------------------------------------
----------- Atenção! A partir daqui --------------
------ medidas drásticas foram tomadas -----------
-------------- Expressions -----------------------


isInScope :: Token -> Scope -> Bool
isInScope (Id _ (l, c)) (_,_,[]) = False
isInScope (Id id1 p1) (i,j,((Id id2 _,val)):t) = if id1 == id2 then True
                                         else isInScope (Id id1 p1) (i,j,t)
isInScope _ _ = False -- Just to avoid error of non-exhaustive...

variable :: ParsecT [Token] CapivaraState IO(Token)
variable =  (do 
                s <- getState
                a <- idToken
                return (getVal a (
                    getCurrentScope(s))))
 
getVal :: Token -> Scope -> Token
getVal (Id _ (l, c)) (_,_,[]) = error ("variable not declared in the scope at line " ++ (show l) ++ " column " ++ (show c))
getVal (Id id1 p1) (i,j,((Id id2 _,val)):t) = if id1 == id2 then val
                                         else getVal (Id id1 p1) (i,j,t)

----------------------------------------
-- Expressions
----------------------------------------
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
                    <|> parenExpr <|> functionCall
                return (a))
 
--- funções considerando associatividade à esquerda                  
bin_term4 :: ParsecT [Token] CapivaraState IO(Token)
bin_term4 = do
                   n1 <- intToken <|> booleanToken <|> floatToken <|> 
                      stringToken <|> variable <|>  
                      try listLiteral <|> try matrixLiteral <|> parenExpr
                      <|> functionCall
                   result <- eval_term4 n1
                   return (result)

eval_term4 :: Token -> ParsecT [Token] CapivaraState IO(Token)
eval_term4 n1 = do
                 op <- powerToken <|> plusplusToken
                 n2 <- intToken <|> booleanToken  <|> floatToken <|> 
                    stringToken <|> variable <|> try listLiteral <|> 
                    try matrixLiteral <|> parenExpr <|> functionCall
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

-- Functions TODO computar o valor
functionCall :: ParsecT [Token] CapivaraState IO(Token)
functionCall = do
          z <- lessToken
          CpvFunc args <- variable
          b <- beginbracketToken
          c <- argsList
          d <- endbracketToken
          w <- greaterToken
          w <- getInput
          setInput args
          i <- beginscopeToken
          n <- stmts
          j <- returnToken
          k <- expression
          l <- semicolonToken
          m <- endscopeToken
          setInput w
          return (k)

argsList = (do
               a <- idToken
               b <- commaToken
               c <- argsList
               return (a:b:c)) <|>
             (do
               a <- idToken
               b <- argsList
               return (a:b)) <|> (return [])

