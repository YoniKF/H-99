-- https://wiki.haskell.org/99_questions/31_to_41

import Data.List (group, foldl', find)

-- 31
isPrime :: Integral a => a -> Bool
isPrime 1 = False
isPrime n = all ((/= 0) . (n `mod`)) $ [2.. truncate . sqrt . fromIntegral $ n]

-- 32
myGCD :: Integral a => a -> a -> a
myGCD m 0 = abs m
myGCD m n = myGCD n (m `mod` n)

-- 33
coprime :: Integral a => a -> a -> Bool
coprime m n = myGCD m n == 1

-- 34
totient :: Integral a => a -> a
totient n = fromIntegral $ length $ filter (coprime n) [1..n]

-- 35
primes :: Integral a => [a]
primes = filter isPrime [2..]

primeFactors :: Integral a => a -> [a]
primeFactors n = helper n primes
    where helper 1 _      = []
          helper n pps@(p:ps)
            | n `mod` p == 0 = p : helper (n `div` p) pps
            | otherwise      = helper n ps

-- 36
primeFactorsMults :: Integral a => a -> [(a,a)]
primeFactorsMults = map (\xs -> (head xs, fromIntegral $ length xs)) . group . primeFactors

-- 37
phi :: Integral a => a -> a
phi = foldl' (\acc (p,m) -> acc * (p - 1) * p ^ (m - 1)) 1 . primeFactorsMults

-- 38 no solution required

-- 39
primesR :: Integral a => a -> a -> [a]
primesR m n = dropWhile (< m) $ takeWhile (<= n) primes

-- 40
goldbach :: Integral a => a -> (a,a)
goldbach n
    | odd n     = error "goldbach: odd number"
    | otherwise = pair
    where Just pair = find (\(p1,p2) -> p1 + p2 == n) [(p1,p2) | p1 <- ps, p2 <- ps]
          ps = primesR 1 n

-- 41
goldbachList :: Integral a => a -> a -> [(a,a)]
goldbachList m n = map goldbach $ filter even [m..n]

goldbachList' :: Integral a => a -> a -> a -> [(a,a)]
goldbachList' m n l = filter (\(a,b) -> a > l && b > l) $ goldbachList m n
