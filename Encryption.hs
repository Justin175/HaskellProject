-- Verschlüsselt eine Liste von ganzen Zahlen
-- Erste Paramenter ist der Öffentliche Schlüssel, hier dargestellt als Tuple
-- Zweite Parameter nimmt die Liste von Zahlen, die Verschlüsselt werden sollen
-- RETURN: Liste verschlüsselter Zahlen
encrypt :: (Integer, Integer) -> [Integer] -> [Integer]
encrypt key []      = []
encrypt key (x:xs)  = encryptSingle key x : encrypt key xs 

-- Erste Paramenter ist der Öffentliche Schlüssel, hier dargestellt als Tuple
-- Zweite Parameter nimmt die Zahl, die Verschlüsselt werden soll
-- RETURN: Verschlüsselte Zahl
encryptSingle :: (Integer, Integer) -> Integer -> Integer
encryptSingle key num = mod (num ^ get_1 key) $ get_2 key

-- Verschlüsselt eine Liste von ganzen Zahlen
-- Erste Paramenter ist der Öffentliche Schlüssel, hier dargestellt als Tuple
-- Zweite Parameter nimmt die Liste von Zahlen, die Verschlüsselt werden sollen
-- RETURN: Liste entschlüsselter Zahlen
decrypt :: (Integer, Integer) -> [Integer] -> [Integer]
decrypt key []      = []
decrypt key (x:xs)  = decryptSingle key x : decrypt key xs 

-- Erste Paramenter ist der Private Schlüssel, hier dargestellt als Tuple
-- Zweite Parameter nimmt die Zahl, die Entschlüsselt werden soll
-- RETURN: Entschlüsselte Zahl
decryptSingle :: (Integer, Integer) -> Integer -> Integer
decryptSingle key cipher = mod (cipher ^ get_1 key) $ get_2 key

get_1 :: (x, y) -> x
get_1 (x, _) = x

get_2 :: (x, y) -> y
get_2 (_, y) = y