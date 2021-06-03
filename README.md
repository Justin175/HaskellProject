# HaskellProject
## Aufgaben
- [ ] Schlüsselgenerierung
- [X] Verschlüsseln
- [X] Entschlüsseln

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
### Ablauf
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

### Algorithmus
Die Verschlüsselung läuft wie folgt ab: Es existierten zwei Methoden, names: encrypt und encryptSingle.
Die EncryptSingle-Funktion nimmt den Öffentlichen Schlüssel und eine Zahl als Parameter, die im folgenden verschlüsselt werden sollen.
Diese verschlüsselte Zahl wird dann im folgenden zurück gegeben. Der Code hierzu sieht wiefolgt aus:

```Haskell
encryptSingle :: (Integer, Integer) -> Integer -> Integer
encryptSingle key num = mod (num ^ get_1 key) $ get_2 key
```

Die Encrypt-Funktion nimmt, im Unterschied zur Encrypt-Funktion, als Parameter den Öffentlichen Schlüssel und eine Liste von Zahlen, 
die verschlüsselt werden sollen. Der Code dieser Funktion sieht wiefolgt aus:

```Haskell
encrypt :: (Integer, Integer) -> [Integer] -> [Integer]
encrypt key []      = []
encrypt key (x:xs)  = encryptSingle key x : encrypt key xs 
```

## Entschlüsseln
