sumTo :: (Num a, Eq a) => a -> [a] -> [(a, a)]
sumTo n xs = xs >>= \x ->
    if n - x `elem` xs then [(x, n - x)] else []

sum3To :: (Num a, Eq a) => a -> [a] -> [(a, a, a)]
sum3To n xs = do
    x <- xs
    x' <- xs
    if n - x - x' `elem` xs then [(x, x', n - x - x')] else []

main = do
    xs <- fmap read . lines <$> readFile "data/day1"
    putStrLn $ f u2 (sumTo 2020 xs)
    putStrLn $ f u3 (sum3To 2020 xs)

    where u2 (x, y) = [x, y]
          u3 (x, y, z) = [x, y, z]
          f u = show . product . u . head
