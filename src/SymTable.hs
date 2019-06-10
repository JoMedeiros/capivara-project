module SymTable where
import Tokens

-- Data Definitions
data Scope = Global         | -- Global variables and constants
             Block Int      | -- For nested blocks
             Function String Int | -- For functions scope: Function function_id call_id 
             FuncBlk String Int Int -- For functions scope: Function function_id call_id 
             deriving (Show, Eq)

data MemCell = Var (Token, Token) -- Variable Token: (id, value, scope_info)
               deriving (Show, Eq)

-- funções para a tabela de símbolos

get_default_value :: Token -> Token
get_default_value (Type "int" (l, c)) = Int 0 (l, c)
get_default_value (Type "float" (l, c)) = Float 0.0 (l, c)
get_default_value (Type "boolean" (l, c)) = Boolean False (l, c)
get_default_value (Type "char" (l, c)) = Char 'a' (l, c)
get_default_value (Type "string" (l, c)) = String "" (l, c)

symtable_insert :: (MemCell) -> [(MemCell)] -> [(MemCell)]
symtable_insert symbol []  = [symbol]
symtable_insert symbol symtable = symtable ++ [symbol]

symtable_update :: (MemCell) -> [(MemCell)] -> [(MemCell)]
symtable_update (Var (Id _ (l,c),_)) [] = error ("Variable not declared in the scope at line " ++ (show l) ++ " column " ++ (show c))
symtable_update (Var (Id id1 p1, v1)) ((Var (Id id2 p2, v2)):t) = 
           if id1 == id2 then (Var (Id id1 p2, v1)) : t
           else (Var (Id id2 p2, v2)) : symtable_update (Var (Id id1 p1, v1)) t

symtable_remove :: (MemCell) -> [(MemCell)] -> [(MemCell)]
symtable_remove _ [] = fail "variable not found"
symtable_remove (Var (id1, v1)) ((Var(id2, v2)):t) = 
                               if id1 == id2 then t
                               else (Var (id2, v2)) : symtable_remove (Var (id1, v1)) t                               

