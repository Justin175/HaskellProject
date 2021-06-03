import System.IO
import Utils

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