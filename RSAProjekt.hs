import System.IO
import Utils
import Encryption

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

main = do
    putStrLn "Willkommen beim RSA-Projekt von Dung Tien Nuygen und Justin Treulieb."

    -- Zuerst wird die Option gewählt, was der Nutzer im weiteren Verlauf des Programmes machen möchte
    option <- wahlDerErstenOption
    putStrLn ("Sie haben die folgende Option gewählt: " ++ option ++ " [" ++ (ersteVersionZuString option) ++ "]")
    putStrLn ""

    --Aus Basis der Option wird jetzt die dazugehörige Methode aufgerufen
    executeOption option

-- Diese Funktion führt je nach gewähler Option, die dazugehörige Methode aus
executeOption option
    | option == "1" = optionVerschluesseln
    | option == "2" = optionEntschluesseln
    | option == "3" = optionGeneriereSchluessel

-- Diese Funktion übernimmt hier die OPTION verschlüsseln:
-- Hier wird nach den Schlüsseln und nach der zu verschlüsselnden Datei gefragt,
-- die im weiteren Verlauf verschlüsselt und neu gespeichert wird.
optionVerschluesseln = do
    -- Name / Pfad der Schlüsseldatei einlesen
    schluesselDatei <- frageNachSchluesselDatei

    -- Name / Pfad der zu verschlüsselnden Datei einlesen
    zuVerschluesselndeDatei <- dateiAbfrage "Welche Datei soll verschlüsselt werden? Geben Sie dazu den Namen / (relativen) Pfad der Datei an."

    -- fragen ob die Ausgabe-Datei automatisch generiert werden soll oder manuell eingeben werden soll
    putStrLn "Geben Sie den Namen der Datei an, in der die verschlüsselten Daten gespeichert werden sollen. Wird hier nichts eingegeben, so wird automatisch ein Name generiert."
    ausgabeDatei <- getLine
    ausgabeDatei <- nichtLeer ausgabeDatei (zuVerschluesselndeDatei ++ ".encry")
    
    putStrLn ""
    putStrLn "Die Verschlüsselung beginnt nun. Dieser Prozess kann einige Zeit in Anspruch nehmen, bitte haben Sie Geduld."
    putStrLn "Infos:"
    putStrLn ("  Schlüssel-Datei            : " ++ schluesselDatei)
    putStrLn ("  Zu Verschlüsselnde-Datei   : " ++ zuVerschluesselndeDatei)
    putStrLn ("  Ausgabe-Datei              : " ++ ausgabeDatei)

    -- Lese den Inhalt der Schlüssel-Datei ein
    schluesselDateiHandle <- openFile schluesselDatei ReadMode
    schluesselDateiContent <- hGetContents schluesselDateiHandle
    hClose schluesselDateiHandle

    putStrLn "Die Verschlüsselung wurde erfolgreich abgeschlossen."

-- Diese Funktion übernimmt hier die OPTION Entschlüsseln:
-- Hier wird nach den Schlüsseln und nach der zu entschlüsselnden Datei gefragt,
-- die im weiteren Verlauf entschlüsselt und neu gespeichert wird.
optionEntschluesseln = do
    -- Name / Pfad der Schlüsseldatei einlesen
    schluesselDatei <- frageNachSchluesselDatei

    -- Name / Pfad der zu entschlüsselnden Datei einlesen
    zuEntschluesselndeDatei <- dateiAbfrage "Welche Datei soll entschlüsselt werden? Geben Sie dazu den Namen / (relativen) Pfad der Datei an."

    -- fragen ob die Ausgabe-Datei automatisch generiert werden soll oder manuell eingeben werden soll
    putStrLn "Geben Sie den Namen der Datei an, in der die entschlüsselten Daten gespeichert werden sollen. Wird hier nichts eingegeben, so wird automatisch ein Name generiert."
    ausgabeDatei <- getLine
    ausgabeDatei <- nichtLeer ausgabeDatei (zuEntschluesselndeDatei ++ ".decry")
    
    putStrLn ""
    putStrLn "Die Verschlüsselung beginnt nun. Dieser Prozess kann einige Zeit in Anspruch nehmen, bitte haben Sie Geduld."
    putStrLn "Infos:"
    putStrLn ("  Schlüssel-Datei            : " ++ schluesselDatei)
    putStrLn ("  Zu Entschlüsselnde-Datei   : " ++ zuEntschluesselndeDatei)
    putStrLn ("  Ausgabe-Datei              : " ++ ausgabeDatei)

    --HIER ENTSCHLÜSSELN HINZUFÜGEN!!!!!!!!!

    putStrLn "Die Entschlüsselung wurde erfolgreich abgeschlossen."

-- Diese Funktion übernimmt hier die OPTION Schlüssengenerierung:
-- Hier wird nach den Schlüsseln und nach der zu entschlüsselnden Datei gefragt,
-- die im weiteren Verlauf entschlüsselt und neu gespeichert wird.
optionGeneriereSchluessel = do
    putStrLn "optionGeneriereSchluessel"

-- Überprüft, ob der erste String 'eingabe' nicht leer ist.
-- Ist dies der Fall wird dieser zurückgegeben, ansonsten wird 'sonstigerName' zurückgegeben
nichtLeer eingabe sonstigerName 
    | eingabe == "" = return sonstigerName
    | otherwise     = return eingabe 

-- Hier wird nach der Datei gefragt, in der sich die / der Schlüssel befindet
frageNachSchluesselDatei = do
    datei <- dateiAbfrage "Geben Sie den Namen / (relativen) Pfad der Datei an, in dem sich der bzw. die Schlüssel befinden."
    return datei

dateiAbfrage nachricht = do 
    putStrLn nachricht
    datei <- getLine
    return datei

-- Hier wird die Ausgabe, der der zuerst wählenden Option, ausgegeben.
-- Hiernach wird die gewählte Option des Nutzers eingelesen und zurückgegeben.
-- Ist die Eingabe ungültig, so wird nach einer erneuten Eingabe gebeten
wahlDerErstenOption = do
        putStrLn "Wählen Sie nun zwischen den folgenden drei Optionen um fortzufahren:"
        putStrLn "[1] Verschlüsseln"
        putStrLn "[2] Entschlüsseln"
        putStrLn "[3] Schlüssel generieren"
        option <- wahlDerErstenOptionAbfrage False
        return option

wahlDerErstenOptionAbfrage fehlerufruf 
    | fehlerufruf = do
        putStrLn "Ihre Eingabe ist ungültig, bitte versuchen Sie es erneut."
        wahlDerErstenOptionAbfrage False
    | otherwise = do
        putStr "Ihre Wahl: "
        option <- getLine
        if not (option == "1" || option == "2" || option == "3") 
            then
                wahlDerErstenOptionAbfrage True
        else return option

ersteVersionZuString :: String -> String
ersteVersionZuString n  | n == "1" = "Verschlüsseln"
                        | n == "2" = "Entschlüsseln"
                        | n == "3" = "Schlüssel generieren"