import Data.Bifunctor (Bifunctor, bimap)

-- Practice B
allPairs :: [(Integer, Integer)]
allPairs = [(x, d - x) | d <- [0..], x <- [0..d]]

-- Practice C
disjoint :: Ord a => [a] -> [a] -> Bool
disjoint _ [] = True
disjoint [] _ = True
disjoint xs'@(x:xs) ys'@(y:ys)
    | x < y = disjoint xs ys'
    | x > y = disjoint xs' ys
    | otherwise = False

-- Practice E
taxiNumber :: Integer -> [(Integer, Integer, Integer, Integer)]
taxiNumber n = [(a, b, c, d) | a <- [1..n]
                            , b <- [a+1..n]
                            , c <- [a+1..n]
                            , d <- [c+1..n]
                            , a^3 + b^3 == c^3 + d^3
                            ]

-- Practice F
data List a = Nil | Snoc (List a) a deriving (Show)

head' :: List a -> a
head' Nil = error "empty list"
head' (Snoc Nil x) = x
head' (Snoc xs _) = head' xs

last' :: List a -> a
last' Nil = error "empty list"
last' (Snoc _ x) = x

toList :: [a] -> List a
-- toList [] = Nil
-- toList [x] = Snoc Nil x
-- toList xs = Snoc (toList (reverse ys)) y
--     where
--         (y:ys) = reverse xs
toList = convert . reverse
    where
        convert [] = Nil
        convert (x:xs) = Snoc (convert xs) x

fromList :: List a -> [a]
-- fromList Nil = []
-- fromList (Snoc Nil x) = [x]
-- fromList (Snoc xs x) = fromList xs ++ [x]
fromList = reverse . convert
    where
        convert Nil = []
        convert (Snoc xs x) = x : convert xs

-- Practice H
take' :: Int -> [a] -> [a]
-- INFO: ここをワイルドパターンにしてしまうと、take' undefined [] が undefined になってしまう
take' n [] = []
take' n (x:xs) = if n == 0 then [] else x : take' (n-1) xs

drop' :: Int -> [a] -> [a]
drop' n [] = []
drop' n (x:xs) = if n == 0 then x:xs else drop' (n-1) xs

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' _ [] = ([], [])
splitAt' n xs'@(x:xs) = if n == 0 then ([], xs')
                        else (x:ys, zs)
                        where
                            (ys, zs) = splitAt' (n-1) xs

-- Practice K
{-
map :: (a -> b) -> [a] -> [b]
fst :: (a, b) -> a
snd :: (a, b) -> b
fork :: (a -> b, a -> c) -> a -> (b, c)
map fst :: [(a, b)] -> [a]
map snd :: [(a, b)] -> [b]
fork (map fst, map snd) :: [(a, b)] -> ([a], [b]) :: unzip

(.) :: (b -> c) -> (a -> b) -> a -> c
fork (f . fst, g . snd) :: (a -> b, c -> d) -> (a, c) -> (b, d) :: cross

cross (map f, map g) . unzip = cross (map f, map g) . fork (map fst, map snd)
                             = fork (map f . map fst, map g . map snd)
                             = fork (map (f . fst), map (g . snd))

unzip . map (cross (f, g)) = fork (map fst, map snd) . map (cross (f, g))
                           = fork (map fst . map (cross (f, g)), map snd . map (cross (f, g)))
                           = fork (map (fst . cross (f, g)), map (snd . cross (f, g)))
                           = form (map (f .fst), map (g . snd))
-}

-- Practice L
{-
cross (f . g) . cross (h, k) = fork (f . fst, g . snd) . fork (h, k)
                            = fork (f . fst . h, g . snd . k)
                            = fork (f . h . fst, g . k . snd)
                            = cross (f . h, g . k)

class Bifunction p where
    bimap :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)

cross :: (a -> b, c -> d) -> (a, c) -> (b, d)

uncurry :: (a -> b -> c) -> (a, b) -> c

uncurry bimap :: (a -> b, c -> d) -> (a, c) -> (b, d)
                = cross
-}

data Either' a b = Left' a | Right' b
instance Bifunctor Either' where
    bimap f g (Left' x) = Left' (f x)
    bimap f g (Right' x) = Right' (g x)

