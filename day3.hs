import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

sanitize :: [Text] -> [[Bool]]
sanitize xs = fmap (cycle . fmap (== '#') . T.unpack) xs

checkA :: [[Bool]] -> Int
checkA xs = length $ filter id $ zipWith (!!) xs [0,3..]

checkB :: [[Bool]] -> (Int, Int) -> Int
checkB xs (r, d) = length $ filter id $ zipWith (!!) (nth d xs) [0,r..]
    where nth n [] = []
          nth n (x:xs) = x : nth n (drop (n - 1) xs)

main = do
    xs <- sanitize . T.lines <$> TIO.readFile "data/day3"
    putStrLn $ show $ checkA xs
    let f = checkB xs
    putStrLn $ show $ f (1, 1) * f (3, 1) * f (5, 1) * f (7, 1) * f (1, 2)

