module Parser.SymbolTable(Entry(..), SymbolTable, ReplInput(..), storeDefinition) where
import Parser.AbstractSyntax

--data SymbolTable = SymbolTable [(String, Definition)] deriving Show
         --   h        VariableDef( Number 41)
         --   a        VariableDef( BinaryExpr (Var h) + 1)
         --   f        FunctionDef(f, [x], BinaryExpr (Var a) + x)    
--data Ee = Ee1 Definition | Ee2 Application | Ee3  
data ReplInput = Def Definition | Exp Expression
type Entry = (String, Definition)
instance Show ReplInput where 
    show (Def d)  = show d 
    show (Exp exp1) = show exp1

type SymbolTable =  [Entry] 

storeDefinition :: Definition -> Entry
storeDefinition (FunctionDef name params body) =  (name, FunctionDef name params body)
storeDefinition (VariableDef name body) = (name, VariableDef name body)



 {-
addNewDefToStoredList :: String -> Definition -> SymbolTable -> SymbolTable
--addNewDefToStoredList name newDef (SymbolTable [(name, oldDef)]) = SymbolTable [((name, oldDef):   (storeDefinition newDef))]
addNewDefToStoredList name newDef symbolTable = symbolTable ++ [Def (name, newDef)]
-}


