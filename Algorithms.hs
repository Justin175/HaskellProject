erweiteter_euclid :: Int -> Int -> (Int, Int, Int)
erweiteter_euclid a b   | b == 0    = (a, 1, 0) 
                        | otherwise = (x, z, y - div a b * z)
                            where
                                reku = erweiteter_euclid b $ mod a b
                                x = get_1 reku
                                y = get_2 reku
                                z = get_3 reku

get_1 :: (x, y, z) -> x
get_1 (x, _, _) = x

get_2 :: (x, y, z) -> y
get_2 (_, y, _) = y

get_3 :: (x, y, z) -> z
get_3 (_, _, z) = z