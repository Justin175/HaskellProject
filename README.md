# HaskellProject
## Aufgaben
- [ ] Schlüsselgenerierung
- [ ] Verschlüsseln
- [ ] Entschlüsseln

## Schlüsselgenerierung
- [ ] Streng Manuelle Eingabe
- [ ] Manuelle Eingabe
- [ ] Automatische Generierung

#### Streng Manuelle Eingabe
- Eingabe p,q,e als Zahlen
- Kontrolle der Eingabe

#### Manuelle Eingabe
- Wahl p und q aus einer Liste
- Wahl von e aus einer Liste<br>
	  phi(n) → Liste [phi(n) * 0.6 .. phi(n) * 0.9] → ermitteln aller teilerfremden zu phi(n) → eine 	Zahl zufällig aus der Liste wählen (=e)
- Kontrolle der Eingabe

####  Automatische Generierung
- p, q, e werden automatisch generiert
- p,q,e werden zufällig aus einer Liste gewählt → Zufall herstelle, z.B. durch LCG

## Verschlüsseln
Angabe des öffentlichen Schlüssels (Datei?, Eingabe in der Konsole)
Prüfung des Schlüssels
> Text eingeben
→ Konervtierung des Textes in eine Liste von Zahlen
→ Liste dieser Zahlen verschlüsseln
→ Diese Liste zurück in einen Text konvertieren (HEX-Darstellung?)
→ Ausgabe: Verschlüsselter Text
> Datei einlesen
…
→ Ausgabe: Verschlüsselte Datei

## Entschlüsseln
