-- Practice A
subtract' :: Num a => a -> a -> a
subtract' = flip (-)

-- Practice B
exp' :: (Fractional a, Integral b) => a -> b -> a
exp' x n = if n >= 0 then x ^ n else 1 / (x ^ negate n)

-- Practice C
div' :: Integral a => a -> a -> a
div' x y = floor (fromIntegral x /  fromIntegral y)

-- Practice E
type Interval = (Integer, Integer)
isqrt :: Float -> Integer
isqrt x
    | x < 0 = error "isqrt: negative argument"
    | otherwise = fst (until unit (shrink x) (bound x))
    where
        unit (m, n) = m + 1 == n

leq :: Integer -> Float -> Bool
x `leq` y = fromIntegral x <= y

lt :: Float -> Integer -> Bool
x `lt` y = x < fromIntegral y

shrink :: Float -> Interval -> Interval
shrink x (m, n) = if (p * p) `leq` x then (p, n) else (m, p)
    where
        p = (m + n) `div` 2

bound :: Float -> Interval
bound x = (0, upper x)
    where
        upper :: Float -> Integer
        upper x = until (x `lt`) (* 2) 1

-- Practice F
sqrt' :: Float -> Float
sqrt' x = until goodEnough improve x
    where
        goodEnough y = abs (y * y - x) < eps * x
        eps = 0.00001
        improve y = (x + y * y) / (2 * y)

-- Practice G
data Nat = Zero | Succ Nat
  deriving (Eq, Show)

instance Ord Nat where
    Zero <= _ = True
    Succ m <= Zero = False
    Succ m <= Succ n = m <= n

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

divMod' :: Nat -> Nat -> (Nat, Nat)
divMod' x y
    | y == Zero = error "divMod': divide by zero"
    | x < y = (Zero, x)
    | otherwise = (Succ q, r)
    where
        (q, r) = divMod' (x - y) y
