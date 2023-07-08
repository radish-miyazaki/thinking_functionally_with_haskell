module CommonWords where

import Data.List (group, sortBy, sort)
import Data.Char (toLower)
import Data.String (words)

commonWords :: Int -> String -> String
commonWords n = concatMap showRun . take n . sortRuns . countRuns .
        sortWords . words . map toLower

sortWords :: [String] -> [String]
sortWords = sort

countRuns :: [String] -> [(Int, String)]
countRuns = map (\xs -> (length xs, head xs)) . group

sortRuns :: [(Int, String)] -> [(Int, String)]
sortRuns = sortBy (\(x, _) (y, _) -> compare y x)

showRun :: (Int, String) -> String
showRun (n, x) = x ++ ": " ++ show n ++ "\n"

