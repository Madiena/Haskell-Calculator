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
* aus dem Ordner `/src/`: `ghci Main.hs`
* in GHCI: `main`