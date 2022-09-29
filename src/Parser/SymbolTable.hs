module Parser.SymbolTable(Entry(..), SymbolTable, ReplInput(..), storeDefinition) where
import Parser.AbstractSyntax


data ReplInput = Def Definition | Exp Expression

type Entry = (String, Definition)

instance Show ReplInput where 
    show (Def d)  = show d 
    show (Exp exp1) = show exp1

type SymbolTable =  [Entry] 

storeDefinition :: Definition -> Entry
storeDefinition (FunctionDef name params body) =  (name, FunctionDef name params body)
storeDefinition (VariableDef name body) = (name, VariableDef name body)


