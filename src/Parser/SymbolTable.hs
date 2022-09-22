module Parser.SymbolTable where
import Parser.AbstractSyntax

data SymbolTable = SymbolTable [(String, Definition)] deriving Show
         --   h        VariableDef( Number 41)
         --   a        VariableDef( BinaryExpr (Var h) + 1)
         --   f        FunctionDef(f, [x], BinaryExpr (Var a) + x)    

storeDefinition :: Definition -> SymbolTable
storeDefinition (FunctionDef name params body) =  SymbolTable [(name, (FunctionDef name params body))]
storeDefinition (VariableDef name body) = SymbolTable [(name, (VariableDef name body))]
    
addNewDefToStoredList :: Definition -> SymbolTable -> SymbolTable
addNewDefToStoredList newDef SymbolTable [(name, oldDef)] = SymbolTable [((name, oldDef): (storeDefinition newDef))]