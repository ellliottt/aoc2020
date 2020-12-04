{-# LANGUAGE OverloadedStrings, DeriveFunctor #-}

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

sanitize :: Text -> [Map Text Text]
sanitize = fmap (M.fromList . fmap (unpack . T.split (== ':')) . T.words) . T.splitOn "\n\n"
    where unpack [x, y] = (x, y)

validA :: Map Text Text -> Bool
validA p = let diff = fields S.\\ M.keysSet p
           in S.null diff || diff == S.singleton "cid"
    where fields = S.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

validB :: Map Text Text -> Bool
validB m = v byr "byr" && v iyr "iyr" && v eyr "eyr" && v hgt "hgt"
        && v hcl "hcl" && v ecl "ecl" && v pid "pid"

    where v :: ReadP a -> Text -> Bool
          v f k = case M.lookup k m of
              Just x -> not $ null $ readP_to_S f (T.unpack x)
              Nothing -> False

          digits :: Int -> ReadP Int
          digits n = read <$> count n (satisfy isDigit)
          range :: Int -> Int -> Int -> ReadP Int
          range i j x = if i <= x && x <= j then pure x else pfail

          byr = digits 4 >>= range 1920 2002
          iyr = digits 4 >>= range 2010 2020
          eyr = digits 4 >>= range 2020 2030
          hgt = do
              n <- read <$> many1 (satisfy isDigit)
              unit <- string "cm" +++ string "in"
              if unit == "cm"
              then range 150 193 n
              else range 59 76 n
          hcl = do
              char '#'
              count 6 $ satisfy (`elem` ("0123456789abcdef" :: String))
          ecl = choice $ fmap string ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
          pid = digits 9 <* eof

main = do
    xs <- sanitize <$> TIO.readFile "data/day4"
    putStrLn $ show $ length $ filter validA xs
    putStrLn $ show $ length $ filter validB xs
