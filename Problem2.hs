import Data.String (unwords, words)
import Data.Char (toUpper)

-- Practice C

exampleTitle :: String
exampleTitle = "The morphology of pres -- an essay in meta-algorithmics"

capitalizeTitle :: String -> String
capitalizeTitle = unwords . map capitalize . words

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

-- Practice D

first :: (a -> Bool) -> [a] -> a
first p xs
    | null xs = error "Empty list"
    | p x = x
    | otherwise = first p (tail xs)
    where
        x = head xs

first' :: (a -> Bool) -> (a -> b) -> [a] -> a
first' p f xs
    | null xs = error "Empty list"
    | p x = x
    | otherwise = first' p f (tail xs)
    where
        x = head xs

-- Practice E

first'' :: (a -> Bool) -> [a] -> Maybe a
first'' p xs = if null xs then Nothing else Just (head ys)
                where
                    ys = filter p xs

-- Practice F

exp' :: Integer -> Integer -> Integer
exp' x n
    | n == 0 = 1
    | n == 1 = x
    | otherwise = x * exp' x (n - 1)

exp'' :: Integer -> Integer -> Integer
exp'' x n
    | n == 0 = 1
    | n == 1 = x
    | even n = exp'' (x * x) (n `div` 2)
    | odd n = x * exp'' (x * x) ((n - 1) `div` 2)

-- Practice G

type Date = (Int, Int, Int)

showDate :: Date -> String
showDate (d, m, y) = show d ++ suffix d ++ " " ++ monthName m ++ ", " ++ show y

monthName :: Int -> String
monthName m
    | m == 1 = "January"
    | m == 2 = "February"
    | m == 3 = "March"
    | m == 4 = "April"
    | m == 5 = "May"
    | m == 6 = "June"
    | m == 7 = "July"
    | m == 8 = "August"
    | m == 9 = "September"
    | m == 10 = "October"
    | m == 11 = "November"
    | m == 12 = "December"
    | otherwise = error "Invalid month"

suffix :: Int -> String
suffix d
    | d == 1 || d == 21 || d == 31 = "st"
    | d == 2 || d == 22 = "nd"
    | d == 3 || d == 23 = "rd"
    | otherwise = "th"

-- Practice H

type CIN = String

addSum :: CIN -> CIN
addSum cin = if length cin == 8 then cin ++ twoDigitsString (sum (map getDigit cin)) else error "Invalid CIN"

twoDigitsString :: Int -> String
twoDigitsString n
    | n < 10 = "0" ++ show n
    | n < 100 = show n
    | otherwise = error "Invalid value"

getDigit :: Char -> Int
getDigit c = read [c] :: Int

valid :: CIN -> Bool
valid cin = (length cin == 10) && (cin == addSum (take 8 cin))
