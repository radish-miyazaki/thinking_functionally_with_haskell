digits2 :: Int -> (Int, Int)
digits2 = (`divMod` 10)

leq ::  Integer -> Float -> Bool
m `leq` x = fromInteger m <= x

lt :: Float -> Integer -> Bool
x `lt` n = x < fromInteger n

tooSLowFloor :: Float -> Integer
tooSLowFloor x = if x < 0
          then until (`leq` x) (subtract 1) (-1)
          else until (x `lt`) (+ 1) 1 - 1

floor :: Float -> Integer
floor x = fst (until unit (shrink x) (bound x))
    where
        unit (m, n) = m + 1 == n

type Interval = (Integer, Integer)

shrink :: Float -> Interval -> Interval
shrink x (m, n) = if p `leq` x then (p, n) else (m, p)
  where
    p = choose (m, n)

choose :: Interval -> Integer
choose (m, n) = (m + n) `div` 2

bound :: Float -> Interval
bound x = (lower x, upper x)

lower :: Float -> Integer
lower x = until (`leq` x) (* 2) (-1)

upper :: Float -> Integer
upper x = until (x `lt`) (* 2) 1

data Nat = Zero | Succ Nat
  deriving (Eq, Ord, Show)

-- strict version
-- data Nat = Zero | Succ !Nat
--   deriving (Eq, Ord, Show)

instance Num Nat where
  m + Zero = m
  m + Succ n = Succ (m + n)

  m * Zero = Zero
  m * (Succ n) = m * n + m

  abs n = n

  signum Zero = Zero
  signum (Succ n) = Succ Zero

  m - Zero = m
  Zero - Succ n = Zero
  Succ m - Succ n = m - n

  fromInteger n
    | n <= 0 = Zero
    | otherwise = Succ (fromInteger (n - 1))

infinity :: Nat
infinity = Succ infinity