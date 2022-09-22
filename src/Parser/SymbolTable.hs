module Parser.SymbolTable where
import Parser.AbstractSyntax

data SymbolTable = SymbolTable [(String, Definition)] deriving Show
         --   h        VariableDef( Number 41)
         --   a        VariableDef( BinaryExpr (Var h) + 1)
         --   f        FunctionDef(f, [x], BinaryExpr (Var a) + x)    

--storeDefinition :: Definition -> SymbolTable
--storeDefinition (FunctionDef name params body) =  SymbolTable (fmap const (name, FunctionDef))
--storeDefinition (VariableDef name body) = SymbolTable (fmap const (name, VariableDef))
    
    
