import System.IO
import System.Directory
import Data.Time.Clock
import Utils
import Encryption
import Schluesselgenerierung


-- --------------- PROGRAMM-ABLAUF (BASIS)
-- Willkommen
-- Optionen wählen
--  1. Verschlüsseln
--  2. Entschlüsseln
--  3. Schlüssel generieren

-- Option wurde gewählt
-- 1./2:    - Datei angeben, indem sich der Schlüssel befindet und einlesen
--          - Datei angeben, die ent- / verschlüsselt werden soll
--          - Fragen: Soll die Output-Datei automatisch generiert werden ? JA -> alterName.ecry (decry) | NEIN -> Name wird angegeben
--          - ver- bzw. entschlüsseln und in neue Datei speichern
--          - ERFOLGREICH :)
--
-- 3.       - Option wählen: datei mit pqe; datei mit pq; automatisch (bei keiner eingabe wird option automatisch gewählt)
--          - AUTOMATISCH
--              - Wo sollen die Schlüssel gespeichert werden?
--              - Schlüssel speichern
--          - Datei pq
--              - Wo ist datei mit pq?
--              - Wo sollen die Schlüssel gespeichert werden?
--              - Schlüssel speichern
--          - Datei pqe
--              - Wo ist datei mit pqe?
--              - Wo sollen die Schlüssel gespeichert werden?
--              - Schlüssel speichern

main = do -- WICHTIG: Wir lesen Verschlüsselungsschlüssel aus einer Datei ein, die beide Schlüssel enthält, was in der Praxis aber nicht der Fall sein kann
-- Also entweder Benutzer Bescheid geben, dass eine leere Zeile entsprechend hin muss oder andere Lösung finden.
-- Erfolgreiche Generierung noch prüfen (also ob e passt)
    putStrLn "Willkommen beim RSA-Projekt von Dung Tien Nuygen und Justin Treulieb."

    -- Zuerst wird die Option gewählt, was der Nutzer im weiteren Verlauf des Programmes machen möchte
    option <- wahlFunktionalitaet
    putStrLn ("Sie haben die folgende Option gewählt: " ++ option ++ " [" ++ (ersteVersionZuString option) ++ "]")
    putStrLn ""

    --Aus Basis der Option wird jetzt die dazugehörige Methode aufgerufen
    optionAusfuehren option

-- Diese Funktion führt je nach gewähler Option, die dazugehörige Methode aus
optionAusfuehren option
    | option == "1" = verschluesseln
    | option == "2" = entschluesseln
    | option == "3" = schluesselGenerieren

-- Diese Funktion übernimmt hier die OPTION verschlüsseln:
-- Hier wird nach den Schlüsseln und nach der zu verschlüsselnden Datei gefragt,
-- die im weiteren Verlauf verschlüsselt und neu gespeichert wird.
verschluesseln = do
    -- Name / Pfad der Schlüsseldatei einlesen
    schluesselDatei <- quelldateiAbfrage "der öffentliche Schlüssel"

    -- Name / Pfad der zu verschlüsselnden Datei einlesen
    zuVerschluesselndeDatei <- quelldateiAbfrage "der zu verschlüsselnde Text"

    -- fragen ob die Ausgabe-Datei automatisch generiert werden soll oder manuell eingeben werden soll
    putStrLn "Geben Sie den Namen der Datei an, in der die verschlüsselten Daten gespeichert werden sollen. Wird hier nichts eingegeben, so wird automatisch eine Datei generiert."
    ausgabeDatei <- getLine
    ausgabeDatei <- generiereAusgabeDatei ausgabeDatei (zuVerschluesselndeDatei ++ ".encry")
    
    putStrLn ""
    putStrLn "Die Verschlüsselung beginnt nun. Dieser Prozess kann einige Zeit in Anspruch nehmen. Bitte haben Sie etwas Geduld."
    putStrLn "Infos:"
    putStrLn ("  Schlüssel-Datei            : " ++ schluesselDatei)
    putStrLn ("  Zu Verschlüsselnde-Datei   : " ++ zuVerschluesselndeDatei)
    putStrLn ("  Ausgabe-Datei              : " ++ ausgabeDatei)

    -- Öffne Schlüssel-Handler und lese content
    schluesselDateiHandle <- openFile schluesselDatei ReadMode
    schluesselDateiContent <- hGetContents schluesselDateiHandle

    -- Öffne VerschlüsselndeDatei-Handler und lese content
    zuVerschluesselndeDateiHandler <- openFile zuVerschluesselndeDatei ReadMode
    zuVerschluesselndeDateiContent <- hGetContents zuVerschluesselndeDateiHandler

    --encrypted <- return getPulblicKeyFromList (stringListToIntegerList (words schluesselDateiContent))
    writeFile ausgabeDatei (integerListToString (encrypt (getPublicKeyFromList (stringListToIntegerList (lines schluesselDateiContent))) (charListToAsciiIntegerList zuVerschluesselndeDateiContent)) " ")

    -- Handler schließen (Schlüsseldatei)
    hClose schluesselDateiHandle
    hClose zuVerschluesselndeDateiHandler

    putStrLn "Die Verschlüsselung wurde erfolgreich abgeschlossen."

-- Diese Funktion übernimmt hier die OPTION Entschlüsseln:
-- Hier wird nach den Schlüsseln und nach der zu entschlüsselnden Datei gefragt,
-- die im weiteren Verlauf entschlüsselt und neu gespeichert wird.
entschluesseln = do
    -- Name / Pfad der Schlüsseldatei einlesen
    schluesselDatei <- quelldateiAbfrage "der private Schlüssel"

    -- Name / Pfad der zu entschlüsselnden Datei einlesen
    zuEntschluesselndeDatei <- quelldateiAbfrage "der zu entschlüsselnde Text"

    -- fragen ob die Ausgabe-Datei automatisch generiert werden soll oder manuell eingeben werden soll
    putStrLn "Geben Sie den Namen der Datei an, in der die entschlüsselten Daten gespeichert werden sollen. Wird hier nichts eingegeben, so wird automatisch eine Datei generiert."
    ausgabeDatei <- getLine
    ausgabeDatei <- generiereAusgabeDatei ausgabeDatei (zuEntschluesselndeDatei ++ ".decry")
    
    putStrLn ""
    putStrLn "Die Entschlüsselung beginnt nun. Dieser Prozess kann einige Zeit in Anspruch nehmen, bitte haben Sie Geduld."
    putStrLn "Infos:"
    putStrLn ("  Schlüssel-Datei            : " ++ schluesselDatei)
    putStrLn ("  Zu Entschlüsselnde-Datei   : " ++ zuEntschluesselndeDatei)
    putStrLn ("  Ausgabe-Datei              : " ++ ausgabeDatei)
    
   -- Öffne Schlüssel-Handler und lese content
    schluesselDateiHandle <- openFile schluesselDatei ReadMode
    schluesselDateiContent <- hGetContents schluesselDateiHandle

    -- Öffne EntschlüsselndeDatei-Handler und lese content
    zuEntschluesselndeDateiHandler <- openFile zuEntschluesselndeDatei ReadMode
    zuEntschluesselndeDateiContent <- hGetContents zuEntschluesselndeDateiHandler

    -- Schreibe das Entschlüsselte
    writeFile ausgabeDatei (integerListToCharString (decrypt (getPrivateKeyFromList (stringListToIntegerList (lines schluesselDateiContent))) (stringListToIntegerList (words zuEntschluesselndeDateiContent))))
    
    -- Handler schließen (Schlüsseldatei)
    hClose schluesselDateiHandle
    hClose zuEntschluesselndeDateiHandler

    putStrLn "Die Entschlüsselung wurde erfolgreich abgeschlossen."

-- Diese Funktion übernimmt hier die OPTION Schlüssengenerierung:
-- Hier wird nach den Schlüsseln und nach der zu entschlüsselnden Datei gefragt,
-- die im weiteren Verlauf entschlüsselt und neu gespeichert wird.
schluesselGenerieren = do
    putStrLn "Wenn Sie ENTER drücken, werden automatisch die beiden Schlüssel generiert."
    putStrLn "Sie können jedoch auch eigene Primzahlen p, q und optional eine Zahl e mit ggT(e, phi(p*q)) = 1 wählen." 
    putStrLn "Dazu geben Sie den Dateinamen an, in der diese Zahlen untereinander stehen"
    primzahlDatei <- getLine
    putStrLn "Ok, geben Sie nun den Namen der Datei ein, in der die Schlüssel gespeichert werden sollen. Andernfalls wird eine Datei automatisch erstellt."
    speicherSchluessel <- getLine
    speicherAuswahl <- generiereAusgabeDatei speicherSchluessel "rsaKeys.txt"

    if(primzahlDatei == "")
        then do
            utc1 <- getCurrentTime
            putStrLn "Schlüssel werden nun generiert"
            utc2 <- getCurrentTime
            putStrLn ("Sie werden in " ++ speicherAuswahl ++ " gespeichert") 
            utc3 <- getCurrentTime

            let rand1 = fromIntegral (rng (utcToInteger (show utc1)) (0, toInteger (length primes - 1)))
                rand2 = fromIntegral (rng (utcToInteger (show utc2)) (0, toInteger (length primes - 1)))
                p = primes !! rand1
                q = primes !! rand2
                phiN = (p - 1) * (q - 1)
                listeTeilerdremdZuPhiN = [x | x <- [2.. (phiN - 1)], get_1 (erweiteterEuklid phiN x) == 1]
                e = listeTeilerdremdZuPhiN !! (fromIntegral (rng (utcToInteger (show utc3)) (0, fromIntegral $ length listeTeilerdremdZuPhiN)))
            
            putStrLn (show p ++ " " ++ show q ++ " " ++ show e)

            schluessenGenerierung [p, q, e] speicherAuswahl
        else do
            primzahlDateiHandle <- openFile primzahlDatei ReadMode
            primzahlDateiContent <- hGetContents primzahlDateiHandle

            let primzahlDateiAlsListe = map (\x -> read x :: Integer) (lines primzahlDateiContent)

            if((length $ primzahlDateiAlsListe) == 2) -- Nur p und q stehen drinne
                then do
                    utc <- getCurrentTime

                    let phiN = (head primzahlDateiAlsListe - 1) * (last primzahlDateiAlsListe - 1)
                    schluessenGenerierung (take 2 primzahlDateiAlsListe ++ (rng (utcToInteger (show utc)) (1, phiN - 1)) : []) speicherAuswahl
  
                else do -- p,q und e stehen drinne
                    schluessenGenerierung (take 3 primzahlDateiAlsListe) speicherAuswahl
            hClose primzahlDateiHandle

    putStrLn "Schlüsselgenerierung abgeschlossen."                


-- Schlüsselgenerations-Option: [Automatisch]
-- Hier wird der Schlüssel für den Nutzer Automatisch generiert
schluessenGenerierung :: [Integer] -> [Char] -> IO ()
schluessenGenerierung [p, q, e] datei = writeFile datei (quadupleToKeyString (generateKeys p q e))


-- Überprüft, ob der erste String 'eingabe' nicht leer ist.
-- Ist dies der Fall wird dieser zurückgegeben, ansonsten wird 'sonstigerName' zurückgegeben
generiereAusgabeDatei :: String -> String -> IO String
generiereAusgabeDatei eingabe sonstigerName 
    | eingabe == "" = return sonstigerName
    | otherwise     = return eingabe 

-- Hier wird nach der Datei gefragt, in der sich die / der Schlüssel befindet
quelldateiAbfrage :: String -> IO String
quelldateiAbfrage quelle = do
    putStrLn ("Geben Sie den Namen / (relativen) Pfad der Datei an, in dem sich " ++ quelle ++ " befindet")
    datei <- getLine
    dateiExistiert <- doesFileExist datei
    if(not dateiExistiert)
        then do
            putStrLn ("Die Datei " ++ datei ++" konnte nicht gefunden werden.")
            quelldateiAbfrage quelle
        else
            return datei

-- Hier wählt der Benutzer die gewünschte Funktionalität aus
-- Ist die Eingabe ungültig, so wird nach einer erneuten Eingabe gebeten
wahlFunktionalitaet :: IO String
wahlFunktionalitaet = do
        putStrLn "Wählen Sie zwischen den folgenden drei Optionen um fortzufahren:"
        putStrLn "[1] Verschlüsseln"
        putStrLn "[2] Entschlüsseln"
        putStrLn "[3] Schlüssel generieren"
        option <- getLine
        if not (option == "1" || option == "2" || option == "3") 
            then do
                putStrLn "Ich kenne nur 1, 2 oder 3"
                wahlFunktionalitaet
        else return option


ersteVersionZuString :: String -> String
ersteVersionZuString n  | n == "1" = "Verschlüsseln"
                        | n == "2" = "Entschlüsseln"
                        | n == "3" = "Schlüssel generieren"

