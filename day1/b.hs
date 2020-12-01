
sum3To :: (Num a, Eq a) => a -> [a] -> [(a, a, a)]
sum3To n xs = do
    x <- xs
    x' <- xs
    if n - x - x' `elem` xs then [(x, x', n - x - x')] else []

main = lines <$> readFile "a.dat" >>= \xs ->
    case sum3To 2020 (fmap read xs) of
        [] -> error "no sum"
        ((x, y, z):_) -> putStrLn $ show $ x * y * z
