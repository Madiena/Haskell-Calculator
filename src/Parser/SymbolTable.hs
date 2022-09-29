module Parser.SymbolTable(SymbolTable, storeDefinition) where

import Parser.AbstractSyntax
    ( Definition(VariableDef, FunctionDef) )

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


