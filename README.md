# Grafikrechner

Projekt realisiert von [Nina Hammer](https://github.com/ninaham) & mir.

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
* `/zeropoints`: nimmt auch ein JSON in der oben angegebenen Form an, welches die zu parsende Funktion enthält und gibt ein JSON zurück weelches die Nullstellen enthält (unter diesem Punkt angegeben). Nullstellen werden nur im Intervall von -10 bis +10 gesucht.
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

### Was in der REPL nicht geht
Zum Veranschaulichen betrachte man folgende Situation:
* `g(x)=x`
* `f(x)=g(x) + 1`
* `f(5)` 

Um `f(5)` zu berechnen wird in die Symboltabelle die Variable `x=5` eingetragen. Um dann `g(x)` zu berechnen wird in die Symboltabelle wieder `x=5` eingetragen. Es ist nicht vorgesehen, dass zwei Variablem mit dem selben Namen in der Symboltabelle stehen. Somit führt diese Eingabe zum Fehlerfall. Folgende Eingabe führt nicht zum Fehlerfall:

* `g(y)=y`
* `f(x)=g(x) + 1`
* `f(5)` 

Nun wird bei der berechnung von `g(x)` keine Variable `x=5` angelegt sondern eine Variable `y=5`. 

Eine mögliche Lösung: wenn `g(x)=x*x+1` in die REPL geschreiben wird, so werden zur weiterverarbeitung die lokalen Variablen umbenannt, sodass sie einzigartig für die Funktion sind. Hier würde also z.B. die Funktion `g(x_g)=x_g*x_g+1` abgespeichert werden. Somit kann es nicht mehr zu Namenskonflikten kommen.
