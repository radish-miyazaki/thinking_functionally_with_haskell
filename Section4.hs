import Data.Char (toLower)
import Data.String (words)
import Data.List (sort)

perfect :: Integral a => a -> Bool
perfect n = n == sum (division n)
    where
        division n = [x | x <- [1..n-1], n `mod` x == 0]

-- triads :: Int -> [(Int, Int, Int)]
-- triads n = [(x, y, z) | x <- [1..n], y<- [1..n], z <- [1..n]
--                       , x * x + y * y == z * z]

divisors :: Integral a => a -> [a]
divisors x = [d | d <- [2..x-1], x `mod` d == 0]

coprime x y = disjoint (divisors x) (divisors y)
    where
        disjoint xs ys = null [x | x <- xs, y <- ys, x == y]

-- triads :: Int -> [(Int, Int, Int)]
-- triads n = [(x, y, z) | x <- [1..n]
--                         , y <- [x+1..n]
--                         , coprime x y
--                         , z <- [y+1..n]
--                         , x * x + y * y == z * z
--                         ]

triads :: Int -> [(Int, Int, Int)]
triads n = [(x, y, z) | x <- [1..m]
                        , y <- [x+1..n]
                        , coprime x y
                        , z <- [y+1..n]
                        , x * x + y * y == z * z
                        ]
                        where
                            m = floor (fromIntegral n / sqrt 2)

data Tree a = Tip a | Fork (Tree a) (Tree a)

instance Functor Tree where
    fmap f (Tip x) = Tip (f x)
    fmap f (Fork u v) = Fork (fmap f u) (fmap f v)

nondec :: (Ord a) => [a] -> Bool
-- nondec [] = True
-- nondec [x] = True
-- nondec (x:y:xs) = (x <= y) && nondec (y:xs)
nondec xs = and (zipWith (<=) xs (tail xs))

position :: (Eq a) => a -> [a] -> Int
position x xs = head ([j | (j, y) <- zip [0..] xs, y == x] ++ [-1])

type Word' = String

commonWords :: Int -> String -> String
commonWords n = concatMap showRun . take n
    . sortRuns . countRuns . sortWords
    . words . map toLower

showRun :: (Int, String) -> String
showRun (n, w) = w ++ ": " ++ show n ++ "\n"

countRuns :: [Word'] -> [(Int, Word')]
countRuns [] = []
countRuns (w:ws) = (1 + length us, w) : countRuns vs
    where
        (us, vs) = span (== w) ws

sortRuns :: [(Int, Word')] -> [(Int, Word')]
sortRuns = reverse . sort

sortWords :: [Word'] -> [Word']
sortWords = sort

sort' :: Ord a => [a] -> [a]
sort' [] = []
sort' [x] = [x]
sort' xs = merge (sort' ys) (sort' zs)
    where
        (ys, zs) = halve xs

halve :: [a] -> ([a], [a])
halve xs = splitAt n xs
    where
        n = length xs `div` 2

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xs'@(x:xs) ys'@(y:ys)
    | x <= y = x : merge xs ys'
    | otherwise = y : merge xs' ys
