module Parser.SymbolTable(Entry, SymbolTable, storeDefinition, updateTable, getEntrys) where

import Parser.AbstractSyntax
    ( Definition(FunctionDef, VariableDef), Expression )

--------------------------------------------------------------------------------------------------------------------

-- DEFINITIONEN FÜR DIE SYMBOLTABELLE

{-
    Ein Entry der Symboltabelle besteht aus einem Tupel aus dem Namen des Symbols und dessen Definition
-}
type Entry = (String, Definition)

{-
    Die Symboltabelle ist eine Liste voin Entries
-}
type SymbolTable =  [Entry] 

{-
    speichert einen neuen Eintrag in der Symboltabelle
-}
storeDefinition :: Definition -> Entry
storeDefinition (FunctionDef name params body) =  (name, FunctionDef name params body)
storeDefinition (VariableDef name body) = (name, VariableDef name body)

updateTable :: Entry -> SymbolTable -> Either String SymbolTable
updateTable (name, VariableDef _ exp) table = updateTableByVariable (getEntrys name table) name table exp
updateTable (name, FunctionDef _ params body) table = updateTableByFunction (getEntrys name table) name table params body

updateTableByVariable :: SymbolTable -> String -> SymbolTable -> Expression -> Either String SymbolTable
updateTableByVariable [] name table exp = Right $ table ++ [(name, VariableDef name exp)]
updateTableByVariable [(_, FunctionDef {})] name _ _ = Left $ name ++ " ist bereits eine Funktion und kann keiner Zahl zugewisen werden"
updateTableByVariable [(_, VariableDef {})] name table exp = Right $ [x | x <- table, fst x /= name] ++ [(name, VariableDef name exp)] -- ersetze

updateTableByFunction :: SymbolTable -> String -> SymbolTable -> [String] -> Expression -> Either String SymbolTable
updateTableByFunction [] name table params body = Right $ table ++ [(name, FunctionDef name params body)]
updateTableByFunction [(_, VariableDef {})] name _ _ _ = Left $ name ++ " ist bereits eine Variable und kann keine Funktion zugewisen bekommen"
updateTableByFunction [(_, FunctionDef {})] name table params body = Right $ [x | x <- table, fst x /= name] ++ [(name, FunctionDef name params body)] -- ersetze

getEntrys :: Eq a => a -> [(a, b)] -> [(a, b)]
getEntrys x list = [(a, b) | (a, b) <- list, x == a]

