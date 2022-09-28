module Parser.SymbolTable where
import Parser.AbstractSyntax

--data SymbolTable = SymbolTable [(String, Definition)] deriving Show
         --   h        VariableDef( Number 41)
         --   a        VariableDef( BinaryExpr (Var h) + 1)
         --   f        FunctionDef(f, [x], BinaryExpr (Var a) + x)    
type SymbolTable =  [(String, Definition)] 

storeDefinition :: Definition -> SymbolTable
storeDefinition (FunctionDef name params body) =  [(name, (FunctionDef name params body))]
storeDefinition (VariableDef name body) = [(name, (VariableDef name body))]


 
addNewDefToStoredList :: String -> Definition -> SymbolTable -> SymbolTable
--addNewDefToStoredList name newDef (SymbolTable [(name, oldDef)]) = SymbolTable [((name, oldDef):   (storeDefinition newDef))]
addNewDefToStoredList name newDef symbolTable = symbolTable ++ [(name, newDef)]


{-

aNDL VariableDef( Number 41) [("a", Number 5),("b", 6),("c", 7)]

[ (VariableDef( Number 41) ),("a", Number 5),("b", 6),("c", 7)]
-}


