import Data.List (sortBy, groupBy, sort)
import Data.Char (toUpper)
import Section1 (convert)

-- Practice F

getWords :: Int -> [String] -> [String]
getWords n xs = [x | x <- xs, length x == n]

type Label = String

addLabel :: String -> (Label, String)
addLabel x = (x, sort x)

sortLabels :: [(Label, String)] -> [(Label, String)]
sortLabels = sortBy (\(_, x) (_, y) -> compare x y)

groupByLabel :: [(Label, String)] -> [(Label, [String])]
groupByLabel = map (\xs -> (fst $ head xs, map snd xs)) . groupBy (\(x, _) (y, _) -> x == y)

showRun :: [(Label, [String])] -> String
showRun = concatMap (\(x, xs) -> x ++ ": " ++ show xs ++ "\n")

anagram :: Int -> [String] -> String
anagram n = showRun . groupByLabel . sortLabels . map addLabel . getWords n

-- Practice G

song :: Int -> String
song n = if n == 0 then "" else song (n - 1) ++ "\n" ++ verse n

manStr :: Int -> String
manStr n = if n == 1 then "man" else "men"

verse :: Int -> String
verse n = line1 n ++ line2 n ++ line3 n ++ line4 n

capitalToUpperCase :: String -> String
capitalToUpperCase (x:xs) = toUpper x : xs

line1 :: Int -> String
line1 n =
    capitalToUpperCase (numAndManStr n ++ " went to mow\n")

line2 :: Int -> String
line2 _ = "Went to mow a meadown\n"

line3 :: Int -> String
line3 n =
    capitalToUpperCase (repeatManStr n) ++ " and his dog\n"

numAndManStr :: Int -> String
numAndManStr n = convert n ++ " " ++ manStr n

repeatManStr :: Int -> String
repeatManStr n
    | n == 1 = "one man"
    | otherwise = numAndManStr n ++ ", " ++ repeatManStr (n - 1)

line4 :: Int -> String
line4 = line2
