module SymTable where
import Tokens

type Enumerate = (String,[Token]) -- enum_name, iDs names list
-- Data Definitions
-- Scope counter, anonymous variables counter, Scope stack, 
-- user defined types list 
-- @TODO create a Token for Enumerate type
type CapivaraState = (Int, Int, [Scope], [Enumerate])

-- current scope ID, parent scope ID, symbols table
type Scope = (Int, Int, [TableEntry])

-- (Token Id Name, Token Type Value)
-- @TODO Change second Token to a most generic data type.
  -- this date type should be a Token of value (ex: IntToken)
  -- or a list of Tokens (in case of functions)
type TableEntry = (Token, Token)

-------------------- State Functions--------------------
initScope :: CapivaraState -> CapivaraState
initScope (sc,vc,[],enums) = (sc+1,vc,[scope],enums) where
        scope = (sc+1, sc, [])
initScope (sc,vc,scs,enums) = (sc+1,vc,scope:scs,enums) where
        scope = (sc+1, sc, [])

popScope :: CapivaraState -> CapivaraState
popScope (sc,vc,[],enums) = (sc-1,vc,[],enums)
popScope (sc,vc,s:scs,enums) = (sc-1,vc,scs,enums)

-- Returning current Scope
getCurrentScope :: CapivaraState -> Scope
getCurrentScope (_,_,(c:cs),_) = c

-- funções para a tabela de símbolos

get_default_value :: Token -> Token
get_default_value (Type "int" (l, c)) = Int 0 (l, c)
get_default_value (Type "float" (l, c)) = Float 0.0 (l, c)
get_default_value (Type "boolean" (l, c)) = Boolean False (l, c)
get_default_value (Type "char" (l, c)) = Char 'a' (l, c)
get_default_value (Type "string" (l, c)) = String "" (l, c)

capivaraStateInsert :: TableEntry -> CapivaraState -> CapivaraState
capivaraStateInsert te (sc,vc,[],enums) = 
      (sc,vc,(capivaraScopeInsert te (0,0,[])):[],enums)
capivaraStateInsert te (sc,vc,s:scopes,enums) = 
      (sc,vc,(capivaraScopeInsert te s):scopes,enums)

capivaraScopeInsert :: TableEntry -> Scope -> Scope
capivaraScopeInsert te (sId, pId, table) = 
  (sId, pId, symtable_insert te table)

symtable_insert :: TableEntry -> [TableEntry] -> [TableEntry]
symtable_insert symbol []  = [symbol]
symtable_insert symbol symtable = symtable ++ [symbol]

capivaraStateUpdate :: TableEntry -> CapivaraState -> CapivaraState
capivaraStateUpdate te (sc,vc,s:scopes,enums) = 
      (sc,vc,(capivaraScopeUpdate te s):scopes,enums)

capivaraScopeUpdate :: TableEntry -> Scope -> Scope
capivaraScopeUpdate te (sId, pId, table) = 
  (sId, pId, symtable_update te table)


symtable_update :: TableEntry -> [TableEntry] -> [TableEntry]
symtable_update ( Id _ (l,c),_) [] = error ("iable not declared in the scope at line " ++ (show l) ++ " column " ++ (show c))
symtable_update ( Id id1 p1, v1) (( Id id2 p2, v2):t) = 
           if id1 == id2 then ( (Id id1 p2, v1)) : t
           else ( Id id2 p2, v2) : symtable_update ( Id id1 p1, v1) t

symtable_remove :: TableEntry -> [TableEntry] -> [TableEntry]
symtable_remove _ [] = fail "variable not found"
symtable_remove (id1, v1) ((id2, v2):t) = 
                               if id1 == id2 then t
                               else ( (id2, v2)) : symtable_remove ( (id1, v1)) t                               

--- Type verification ---
get_type :: Token -> [TableEntry] -> Token
get_type _ [] = error "variable not found"
get_type (Id id1 p1) ((Id id2 _, value):t) = if id1 == id2 then value
                                             else get_type (Id id1 p1) t

compatible :: Token -> Token -> Bool
compatible (Int _ _) (Int _ _) = True
compatible (Float _ _) (Float _ _) = True
compatible (Boolean _ _) (Boolean _ _) = True
compatible (String _ _) (String _ _) = True
--compatible _ _ = False
compatible (Int _ (l,c)) _ = error $ "Type mismatch in line " ++ (show l) ++ " column " ++ (show c)
compatible (Float _ (l,c)) _ = error $ "Type mismatch in line " ++ (show l) ++ " column " ++ (show c)
compatible (Boolean _ (l,c)) _ = error $ "Type mismatch in line " ++ (show l) ++ " column " ++ (show c)
compatible (String _ (l,c)) _ = error $ "Type mismatch in line " ++ (show l) ++ " column " ++ (show c)


