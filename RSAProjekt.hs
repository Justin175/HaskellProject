import System.IO
import System.Directory
import Data.Time.Clock
import Utils
import Encryption
import Schluesselgenerierung

main = do 
    putStrLn "Willkommen beim RSA-Projekt von Dung Tien Nuygen und Justin Treulieb."

    -- Zuerst wird die Option gewählt, was der Nutzer im weiteren Verlauf des Programmes machen möchte
    option <- wahlFunktionalitaet
    putStrLn ""

    --Aus Basis der Option wird jetzt die dazugehörige Funktion aufgerufen
    optionAusfuehren option

-- Diese Funktion führt je nach gewähler Option, die dazugehörige Funktion aus
optionAusfuehren option
    | option == "1" = verschluesseln
    | option == "2" = entschluesseln
    | option == "3" = schluesselGenerieren

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
                putStrLn "Geben Sie bitte nur 1, 2 oder 3 ein"
                wahlFunktionalitaet
        else return option


-- Diese Funktion übernimmt hier die OPTION verschlüsseln:
-- Hier wird nach den Schlüsseln und nach der zu verschlüsselnden Datei gefragt,
-- die im weiteren Verlauf verschlüsselt und neu gespeichert wird.
verschluesseln = do
    -- Name / Pfad der Schlüsseldatei einlesen
    schluesselDatei <- quelldateiAbfrage "der öffentliche Schlüssel"

    -- Name / Pfad der zu verschlüsselnden Datei einlesen
    zuVerschluesselndeDatei <- quelldateiAbfrage "der zu verschlüsselnde Text"

    -- fragen ob die Ausgabe-Datei automatisch generiert werden soll oder manuell eingeben wird
    putStrLn "Geben Sie den Namen der Datei an, in der die verschlüsselten Daten gespeichert werden sollen. Wird hier nichts eingegeben, so wird automatisch eine Datei generiert."
    ausgabeDatei <- getLine
    ausgabeDatei <- generiereAusgabeDatei ausgabeDatei (zuVerschluesselndeDatei ++ ".encry")
    
    -- Informationen werden angezeigt
    putStrLn ""
    putStrLn "Die Verschlüsselung beginnt nun. Dieser Prozess kann einige Zeit in Anspruch nehmen. Bitte haben Sie etwas Geduld."
    putStrLn "Infos:"
    putStrLn ("  Schlüssel-Datei            : " ++ schluesselDatei)
    putStrLn ("  Zu Verschlüsselnde-Datei   : " ++ zuVerschluesselndeDatei)
    putStrLn ("  Ausgabe-Datei              : " ++ ausgabeDatei)

    -- Öffne Schlüssel-Handle und lese den Inhalt
    schluesselDateiHandle <- openFile schluesselDatei ReadMode
    schluesselDateiContent <- hGetContents schluesselDateiHandle

    -- Öffne VerschlüsselndeDatei-Handle und lese den Inhalt
    zuVerschluesselndeDateiHandle <- openFile zuVerschluesselndeDatei ReadMode
    zuVerschluesselndeDateiContent <- hGetContents zuVerschluesselndeDateiHandle

    -- Schreibt den verschlüsselten Inhalt in die Ausgabe-Datei. Die Zeichen, die als verschlüsselte Zahlen codiert wurden, werden dabei mit einem Leerzeichen getrennt
    writeFile ausgabeDatei (integerListToString (encrypt (getPublicKeyFromList (stringListToIntegerList (lines schluesselDateiContent))) (charListToAsciiIntegerList zuVerschluesselndeDateiContent)) " ")

    -- Handles schließen 
    hClose schluesselDateiHandle
    hClose zuVerschluesselndeDateiHandle

    putStrLn "Die Verschlüsselung wurde erfolgreich abgeschlossen."

-- Diese Funktion übernimmt hier die OPTION Entschlüsseln:
-- Hier wird nach den Schlüsseln und nach der zu entschlüsselnden Datei gefragt,
-- die im weiteren Verlauf entschlüsselt und neu gespeichert wird.
entschluesseln = do
    -- Name / Pfad der Schlüsseldatei einlesen
    schluesselDatei <- quelldateiAbfrage "der private Schlüssel"

    -- Name / Pfad der zu entschlüsselnden Datei einlesen
    zuEntschluesselndeDatei <- quelldateiAbfrage "der zu entschlüsselnde Text"

    -- fragen ob die Ausgabe-Datei automatisch generiert werden soll oder manuell eingeben wird
    putStrLn "Geben Sie den Namen der Datei an, in der die entschlüsselten Daten gespeichert werden sollen. Wird hier nichts eingegeben, so wird automatisch eine Datei generiert."
    ausgabeDatei <- getLine
    ausgabeDatei <- generiereAusgabeDatei ausgabeDatei (zuEntschluesselndeDatei ++ ".decry")
    
    -- Informationen werden angezeigt
    putStrLn ""
    putStrLn "Die Entschlüsselung beginnt nun. Dieser Prozess kann einige Zeit in Anspruch nehmen, bitte haben Sie Geduld."
    putStrLn "Infos:"
    putStrLn ("  Schlüssel-Datei            : " ++ schluesselDatei)
    putStrLn ("  Zu Entschlüsselnde-Datei   : " ++ zuEntschluesselndeDatei)
    putStrLn ("  Ausgabe-Datei              : " ++ ausgabeDatei)
    
   -- Öffne Schlüssel-Handle und lese den Inhalt
    schluesselDateiHandle <- openFile schluesselDatei ReadMode
    schluesselDateiContent <- hGetContents schluesselDateiHandle

    -- Öffne EntschlüsselndeDatei-Handle und lese den Inhalt
    zuEntschluesselndeDateiHandle <- openFile zuEntschluesselndeDatei ReadMode
    zuEntschluesselndeDateiContent <- hGetContents zuEntschluesselndeDateiHandle
    
    -- Schreibt den entschlüsselten Inhalt in die Ausgabe-Datei
    writeFile ausgabeDatei (integerListToCharString (decrypt (getPrivateKeyFromList (stringListToIntegerList (lines schluesselDateiContent))) (stringListToIntegerList (words zuEntschluesselndeDateiContent))))

    -- Handles schließen 
    hClose schluesselDateiHandle
    hClose zuEntschluesselndeDateiHandle

    putStrLn "Die Entschlüsselung wurde erfolgreich abgeschlossen."

-- Diese Funktion übernimmt hier die OPTION Schlüssengenerierung:
-- Hier wird nach den Schlüsseln und nach der zu entschlüsselnden Datei gefragt,
-- die im weiteren Verlauf entschlüsselt und neu gespeichert wird.
schluesselGenerieren = do
    putStrLn "Wenn Sie jetzt ENTER drücken, werden automatisch die beiden Schlüssel generiert."
    putStrLn "Sie können jedoch auch eigene Primzahlen p, q und optional eine Zahl e mit ggT(e, phi(p*q)) = 1 wählen." 
    primzahlDatei <- quelldateiAbfrage "die Aufzählung dieser Zahlen"
    putStrLn "Ok, geben Sie nun den Namen der Datei ein, in der der öffentliche Schlüssel gespeichert werden soll. Andernfalls wird eine Datei automatisch erstellt."
    publicKeyInput <- getLine
    publicKeyFile <- generiereAusgabeDatei publicKeyInput "publicKey.txt"
    putStrLn "Ok, geben Sie nun den Namen der Datei ein, in der der private Schlüssel gespeichert werden soll. Andernfalls wird eine Datei automatisch erstellt."
    privateKeyInput <- getLine
    privateKeyFile <- generiereAusgabeDatei privateKeyInput "privateKey.txt"

    
    if(primzahlDatei == "") -- Zahlen werden zufällig ausgewählt
        then do
            -- Zwischen den einzelnen getCurrentTime wird eine Funktion eingebaut, damit die utc-Werte nicht identisch sind
            utc1 <- getCurrentTime
            putStrLn "Schlüssel werden nun generiert"
            utc2 <- getCurrentTime
            putStrLn ("Sie werden in " ++ publicKeyFile ++ " und "++ privateKeyFile ++ " gespeichert") 
            utc3 <- getCurrentTime

            -- Bestimmen von Zufallszahlen, mit denen die Primzahl
            let rand1 = fromIntegral (rng (utcToInteger (show utc1)) (0, toInteger (length primes - 1)))
                rand2 = fromIntegral (rng (utcToInteger (show utc2)) (0, toInteger (length primes - 1)))
                p = primes !! rand1
                q = primes !! rand2
                phiN = (p - 1) * (q - 1)
                --Erstellt eine Liste von Zahlen, die teilerfremd zu phiN sind
                listeTeilerdremdZuPhiN = [x | x <- [2.. (phiN - 1)], get_1 (erweiteterEuklid phiN x) == 1]
                e = listeTeilerdremdZuPhiN !! (fromIntegral (rng (utcToInteger (show utc3)) (0, fromIntegral $ length listeTeilerdremdZuPhiN)))
            

            schluesselGenerierung [p, q, e] publicKeyFile privateKeyFile
            putStrLn "Schlüsselgenerierung abgeschlossen."
        else do
            primzahlDateiHandle <- openFile primzahlDatei ReadMode
            primzahlDateiContent <- hGetContents primzahlDateiHandle

            let primzahlDateiAlsListe = map (\x -> read x :: Integer) (lines primzahlDateiContent)

            if((length $ primzahlDateiAlsListe) == 2) -- Nur p und q stehen in der Datei
                then do
                    utc <- getCurrentTime

                    let phiN = (head primzahlDateiAlsListe - 1) * (last primzahlDateiAlsListe - 1)
                        listeTeilerdremdZuPhiN = [x | x <- [2.. (phiN - 1)], get_1 (erweiteterEuklid phiN x) == 1]
                        e = listeTeilerdremdZuPhiN !! (fromIntegral (rng (utcToInteger (show utc)) (0, fromIntegral $ length listeTeilerdremdZuPhiN)))
                    
                    schluesselGenerierung (take 2 primzahlDateiAlsListe ++ e : []) publicKeyFile privateKeyFile
                    hClose primzahlDateiHandle
                    putStrLn "Schlüsselgenerierung abgeschlossen."
                else do -- p,q und e stehen in der Datei
                    eIstOk <- schluesselGenerierung (take 3 primzahlDateiAlsListe) publicKeyFile privateKeyFile
                    if(eIstOk)
                        then do
                            hClose primzahlDateiHandle
                            putStrLn "Schlüsselgenerierung abgeschlossen."
                        else do
                            putStrLn "Die Zahl e erfüllt nicht die erforderlichen Bedingungen"
                            putStrLn "Schlüsselgenerierung fehlgeschlagen. Bitte versuchen Sie es erneut"
                            schluesselGenerieren

                    



-- Schlüsselgenerations-Option: [Automatisch]
-- Hier wird der Schlüssel für den Nutzer Automatisch generiert
schluesselGenerierung :: [Integer] -> [Char] -> [Char] -> IO Bool
schluesselGenerierung [p, q, e] pub priv = do
                                        let res = generateKeys p q e
                                            pubKey = show (get_1Q res) ++ "\n" ++ (show (get_3Q res))
                                            privKey = show (get_2Q res) ++ "\n" ++ (show (get_3Q res))
                                        writeFile pub pubKey
                                        writeFile priv privKey
                                        return (get_4Q res)


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
    if(datei == "")
        then
            return datei
        else do
            dateiExistiert <- doesFileExist datei
            if(not dateiExistiert)
                then do
                    putStrLn ("Die Datei " ++ datei ++" konnte nicht gefunden werden.")
                    quelldateiAbfrage quelle
                else
                    return datei





