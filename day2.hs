
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Bits (xor)

type PasswordRecord = (Char, Int, Int, Text)

sanitize :: Text -> PasswordRecord
sanitize x =
    let (i, r1) = T.span isDigit x
        (j, r2) = T.span isDigit (T.tail r1)
        c       = r2 `T.index` 1
        y       = T.drop 4 r2
    in (c, read (T.unpack i) :: Int, read (T.unpack j) :: Int, y)

validA :: PasswordRecord -> Bool
validA (c, i, j, x) = (x `T.index` (i - 1) == c) `xor` (x `T.index` (j - 1) == c)

validB :: PasswordRecord -> Bool
validB (c, i, j, x) = let n = T.foldl (\n c' -> if c == c' then n + 1 else n) 0 x
                      in i <= n && n <= j

main = do
    xs <- TIO.readFile "data/day2"
    putStrLn $ f validA xs
    putStrLn $ f validB xs

    where f v xs = show $ length $ filter id $ fmap (v . sanitize) $ T.lines xs
