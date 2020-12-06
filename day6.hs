{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Set (Set)
import qualified Data.Set as S

parse :: Text -> [[Text]]
parse = fmap T.lines . T.splitOn "\n\n"

reduce :: ([Set Char] -> Set Char) -> [[Text]] -> Int
reduce f = sum . fmap (S.size . f . fmap (S.fromList . T.unpack))

partA :: [[Text]] -> Int
partA = reduce S.unions

partB :: [[Text]] -> Int
partB = reduce $ foldr1 S.intersection

main = do
    xs <- parse <$> TIO.readFile "data/day6"
    putStrLn $ show $ partA xs
    putStrLn $ show $ partB xs

