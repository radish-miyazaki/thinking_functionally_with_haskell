import Section1 (commonWords)

sqr :: Integer -> Integer
sqr x = x * x

inf :: Integer
inf = 1 + inf

three :: Integer -> Integer
three x = 3

factorical :: Integer -> Integer
factorical n = fact (n, 1)

fact :: (Integer, Integer) -> Integer
fact (x, y) = if x == 0 then y else fact (x - 1, x * y)

to' :: Bool -> Bool
to' b = not (to' b)

cwords :: Int -> FilePath -> FilePath -> IO ()
cwords n infile outfile
    = do { text <- readFile infile
         ; writeFile outfile (commonWords n text)
         ; putStrLn "cwords done!"
         }