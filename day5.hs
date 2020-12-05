{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (sort)

bsp :: (Int, Int, Int) -> Char -> Char -> Text -> Int
bsp i c1 c2 x = unpack $ T.foldl f i x
    where f (n, u, l) c
              | c == c1 = (n `div` 2, u, l - n)
              | c == c2 = (n `div` 2, u + n, n)
          unpack (_, x, _) = x

row, col :: Text -> Int
row = bsp (64, 0, 127) 'F' 'B'
col = bsp (4, 0, 7) 'L' 'R'

seatId :: Text -> Int
seatId x = let (r, c) = T.splitAt 7 x
           in 8 * row r + col c

partA xs = maximum $ fmap seatId xs

partB xs = f $ sort $ fmap seatId xs
    where f (x:y:ys)
              | x + 1 == y = f (y:ys)
              | otherwise = x + 1

main = do
    xs <- T.lines <$> TIO.readFile "data/day5"
    putStrLn $ show $ partA xs
    putStrLn $ show $ partB xs
