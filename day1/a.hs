sumTo :: (Num a, Eq a) => a -> [a] -> [(a, a)]
sumTo n xs = xs >>= \x ->
    if n - x `elem` xs then [(x, n - x)] else []

main = lines <$> readFile "a.dat" >>= \xs ->
    case sumTo 2020 (fmap read xs) of
        [] -> error "no sum"
        ((x, y):_) -> putStrLn $ show $ x * y
