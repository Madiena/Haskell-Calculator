# Grafikrechner

## Starten des Webservers

* Bauen des Projekts und starten des Webservers: ```stack run```
* Der Server startet unter `http://localhost:3000`

## Verfügbare API Requests
* `/function`: nimmt ein JSON engegen mit dem zu parsenden String und gibt ein JSON mit dem geparsten JavaScript Code zurück (auch in der unten angegebenen Form)  
```JSON
 Eingabe / Rückgabe {
	"fnString": String
 }
```
* `/zeropoints`: nimmt auch ein JSON in der oben angegebenen Form an, welches die zu parsende Funktion enthält und gibt ein JSON zurück weelches die Nullstellen enthält (unter diesem Punkt angegeben)
```JSON
Eingabe: {
	"funString": String,
    "vars": [String]
}

Rückgabe: {
	"zeroPoints": [String]
}
```
* `/calculate`: nimmt ein JSON wie in `/zeropoints` entgegen und gibt ein JSON zurück, welches das Ergebnis der Berechnung zurück
```JSON
Eingabe: {
	"funString": String,
    "vars": [String]
}

Rückgabe: {
	"res": Double
}
```

## Nutzen der REPL
* Aus dem Ordner `/src/`starten: `ghci Main.hs` in Terminal eingeben
* In GHCI `main` eingeben
* Nun den gewünschten Mathematischen Ausdruck eingeben: Entweder eine Definition oder einen Ausdruck. 

### Beispiel für REPL
Man gebe die folgenden grau hinterlegten Angaben vor dem "--" in die REPL ein:
* `h = 40` -- `h` wird definiert als die Zahl 40
* `h` -- gibt den Wert von `h` in der REPL zurück
* `g = h + 2` -- weist g den Wert von `h + 2` zu
* `g` -- es kommt 42 zurück
* `h = 10`
* `g` -- es kommt 12 zurück
* `f(x)=x*x`
* `f(5)`
* `f(x)=x+g`
* `f(5)` 
