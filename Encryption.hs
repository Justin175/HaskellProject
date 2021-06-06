module Encryption
where

-- Verschlüsselt eine Liste von ganzen Zahlen
-- Erste Paramenter ist der Öffentliche Schlüssel, hier dargestellt als Tuple
-- Zweite Parameter nimmt die Liste von Zahlen, die Verschlüsselt werden sollen
-- RETURN: Liste verschlüsselter Zahlen
encrypt :: (Integer, Integer) -> [Integer] -> [Integer]
encrypt key []      = []
encrypt key list    = map (encryptSingle key) list

-- Erste Paramenter ist der Öffentliche Schlüssel, hier dargestellt als Tuple
-- Zweite Parameter nimmt die Zahl, die Verschlüsselt werden soll
-- RETURN: Verschlüsselte Zahl
encryptSingle :: (Integer, Integer) -> Integer -> Integer
encryptSingle (e, n) num = mod (num ^ e) n

-- Verschlüsselt eine Liste von ganzen Zahlen
-- Erste Paramenter ist der Öffentliche Schlüssel, hier dargestellt als Tuple
-- Zweite Parameter nimmt die Liste von Zahlen, die Verschlüsselt werden sollen
-- RETURN: Liste entschlüsselter Zahlen
decrypt :: (Integer, Integer) -> [Integer] -> [Integer]
decrypt key []      = []
decrypt key list    = map (decryptSingle key) list

-- Erste Paramenter ist der Private Schlüssel, hier dargestellt als Tuple
-- Zweite Parameter nimmt die Zahl, die Entschlüsselt werden soll
-- RETURN: Entschlüsselte Zahl
decryptSingle :: (Integer, Integer) -> Integer -> Integer
decryptSingle (d, n) cipher = mod (cipher ^ d) n