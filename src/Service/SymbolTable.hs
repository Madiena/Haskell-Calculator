module Service.SymbolTable (Entry, SymbolTable, storeDefinition, updateTable, getEntrys) where

import Service.AbstractSyntax
  ( Definition (FunctionDef, VariableDef),
    Expression,
  )

--------------------------------------------------------------------------------------------------------------------

-- DEFINITIONEN FÜR DIE SYMBOLTABELLE

{-
    Ein Entry der Symboltabelle besteht aus einem Tupel aus dem Namen des Symbols und dessen Definition
-}
type Entry = (String, Definition)

{-
    Die Symboltabelle ist eine Liste voin Entries
-}
type SymbolTable = [Entry]

{-
    Wandelt eine Definition in einen Eintrag um, welcher später in der Symboltabelle gespeichert werden kann
-}
storeDefinition :: Definition -> Entry
storeDefinition (FunctionDef name params body) = (name, FunctionDef name params body)
storeDefinition (VariableDef name body) = (name, VariableDef name body)

{-
    Nimmt einen Eintrag und eine Symboltabelle entgegen. Gibt Symboltabelle mit dem neuen eintrag zurück oder einen Fehler im Either. 
    Bsp. für Fehler: eine bereits bestehenden Funktion soll zu einer Zahl umzudefiniert werden
-}
updateTable :: Entry -> SymbolTable -> Either String SymbolTable
updateTable (name, VariableDef _ exp) table = updateTableByVariable (getEntrys name table) name table exp
updateTable (name, FunctionDef _ params body) table = updateTableByFunction (getEntrys name table) name table params body


{-
    Schreibt in Symboltabelle (3. Argument) eine neue Variablendefinition und gibt vervollständigte Tabelle im Erfolgsfall zurück
    Das erste Argument sind alle Einträge in der Symboltabelle (3. Argument), die den gleichen Namen haben wie der übergebene Name (2. Argument)
    Es wird ein Fehlerstring im Either zurück gegeben, falls versucht wird einer bereits bestehenden Funktion eine Variablendefinition zuzuweisen 
-}
updateTableByVariable :: SymbolTable -> String -> SymbolTable -> Expression -> Either String SymbolTable
updateTableByVariable [] name table expr = Right $ table ++ [(name, VariableDef name expr)]
updateTableByVariable [(_, FunctionDef {})] name _ _ = Left $ name ++ " ist bereits eine Funktion und kann keiner Zahl zugewisen werden"
updateTableByVariable [(_, VariableDef {})] name table exp = Right $ [x | x <- table, fst x /= name] ++ [(name, VariableDef name exp)]
updateTableByVariable _ _ _ _ = Left "Das sollte niemals eintreten"


{-
    Analog zu updateTableByVariable nur für Funktionen
-}
updateTableByFunction :: SymbolTable -> String -> SymbolTable -> [String] -> Expression -> Either String SymbolTable
updateTableByFunction [] name table params body = Right $ table ++ [(name, FunctionDef name params body)]
updateTableByFunction [(_, VariableDef {})] name _ _ _ = Left $ name ++ " ist bereits eine Variable und kann keine Funktion zugewisen bekommen"
updateTableByFunction [(_, FunctionDef {})] name table params body = Right $ [x | x <- table, fst x /= name] ++ [(name, FunctionDef name params body)]
updateTableByFunction _ _ _ _ _ = Left "Das sollte niemals eintreten"


{-
    Gibt Liste mit Paaren zurück, wobei das erste Element der Paare gleich dem ersten Argument ist
-}
getEntrys :: Eq a => a -> [(a, b)] -> [(a, b)]
getEntrys x list = [(a, b) | (a, b) <- list, x == a]
