type Matrix a = [Row a]
type Row a = [a]

type Grid = Matrix Digit
type Digit = Char

type Choices = [Digit]

digits :: [Char]
digits = ['1'..'9']

blank :: Digit -> Bool
blank = (== '0')

solve :: Grid -> [Grid]
solve = filter valid . expand . many prune . choices

prune :: [Row Choices] -> [Row Choices]
prune = pruneBy boxs . pruneBy cols . pruneBy rows

pruneBy :: ([Row Choices] -> [Row Choices]) -> [Row Choices] -> [Row Choices]
pruneBy f = f . map pruneRow . f

many :: (Eq a) => (a -> a) -> a -> a
many f x = if x == y then x else many f y
    where
        y = f x

choices :: Grid -> Matrix Choices
choices = map (map choice)

choice :: Digit -> Choices
choice d = if blank d then digits else [d]

expand :: Matrix Choices -> [Grid]
expand = cp . map cp

valid :: Grid -> Bool
valid g = all nodups (rows g)
       && all nodups (cols g)
       && all nodups (boxs g)

nodups :: (Eq a) => [a] -> Bool
nodups [] = True
nodups (x:xs) = notElem x xs && nodups xs

rows :: Matrix a -> Matrix a
rows = id

cols :: Matrix a -> Matrix a
cols [xs] = [[x] | x <- xs]
cols (xs:xss) = zipWith (:) xs (cols xss)

boxs :: Matrix a -> Matrix a
boxs = map ungroup . ungroup . map cols . group . map group

{-
boxs . boxs = map ungroup . ungroup . map cols . group . map group
            . map ungroup . ungroup . map cols . group . map group
            = map ungroup . ungroup . map cols . group . ungroup . map cols . group . map group
            = map ungroup . ungroup . map cols . map cols . group . map group
            = map ungroup . ungroup . group . map group
            = map ungroup . map group
            = id
-}

group :: [a] -> [[a]]
group [] = []
group xs = take 3 xs : group (drop 3 xs)

ungroup :: [[a]] -> [a]
ungroup = concat

{-
cp [xs] = cp (xs : [])
        = [x : ys | x <- xs, ys <- cp []]
        = [x : ys | x <- xs, ys <- [[]]]
        = [x : [] | x <- xs]
        = [[x] | x <- xs]
-}

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [x : ys | x <- xs, ys <- yss]
    where
        yss = cp xss

pruneRow :: Row Choices -> Row Choices
pruneRow row = map (remove fixed) row
    where
        fixed :: Choices
        fixed = [d | [d] <- row]

remove :: Choices -> Choices -> Choices
remove ds [x] = [x]
remove ds xs = filter (`notElem` ds) xs

{-
f . f = id
map f . filter p . map f = map f . map f . filter (p . f)
                         = map (f . f) . filter (p . f)
                         = filter (p . f)
-}

{-
filter valid . expand = filter (all nodups (rows g) && all nodups (cols g) && all nodups (boxs g)) . expand
                      = filter (all nodups (rows g)) . filter (all nodups (cols g)) . filter (all nodups (boxs g)) . expand
-}

{-
filter (all nodups . boxs) . expand = map boxs . filter (all nodups) . map boxs . expand
                                    = map boxs . filter (all nodups) . expand . boxs
                                    = map boxs . filter (all nodups) . cp . map cp . boxs
                                    = map boxs . cp . map (filter nodups) . map cp . boxs
                                    = map boxs . cp . map (filter nodups . cp) . boxs
                                    = map boxs . cp . map (filter nodups . cp . pruneRow) . boxs
                                    = map boxs . cp . map (filter nodups) . map (cp . pruneRow) . boxs
                                    = map boxs . filter (all nodups) . cp . map (cp . pruneRow) . boxs
                                    = map boxs . filter (all nodups) . cp . map cp . map pruneRow . boxs
                                    = map boxs . filter (all nodups) . expand . map pruneRow . boxs
                                    = filter (all nodups . boxs) . map boxs . expand . map pruneRow . boxs
                                    = filter (all nodups . boxs) . expand . boxs . map pruneRow . boxs
                                    = filter (all nodups . boxs) . expand . pruneBy . boxs

pruneBy f = f . map pruneRow . f
-}