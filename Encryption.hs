module Encryption
where

-- Verschlüsselt eine Liste von ganzen Zahlen
-- Der erste Paramenter ist der öffentliche Schlüssel, hier dargestellt als ein Tupel
-- Der zweite Parameter ist die Liste von Zahlen, die verschlüsselt werden sollen
-- RETURN: Liste verschlüsselter Zahlen
encrypt :: (Integer, Integer) -> [Integer] -> [Integer]
encrypt key []      = []
encrypt key list    = map encryptSingle list
                        where
                            e = fst key
                            n = snd key
                            encryptSingle = (\x -> mod (x ^ e) n)

-- Verschlüsselt eine Liste von ganzen Zahlen
-- Der erste Paramenter ist der private Schlüssel, hier dargestellt als ein Tupel
-- Der zweite Parameter ist die Liste von Zahlen, die entschlüsselt werden sollen
-- RETURN: Liste entschlüsselter Zahlen
decrypt :: (Integer, Integer) -> [Integer] -> [Integer]
decrypt key []      = []
decrypt key list    = map decryptSingle list
                        where
                            d = fst key
                            n = snd key
                            decryptSingle = (\x -> mod (x ^ d) n)