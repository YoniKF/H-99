-- https://wiki.haskell.org/99_questions/21_to_28

import System.Random (randomRs, getStdGen, randomRIO)
import Data.List ((\\), sortBy, groupBy)
import Data.Function (on)

-- 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = pre ++ [x] ++ suf
    where (pre, suf) = splitAt (n - 1) xs

-- 22
range :: Integral a => a -> a -> [a]
range m n = [m..n]

-- 23
rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n = do
    gen <- getStdGen
    return $ map (xs !!) $ take n $ randomRs (0, length xs - 1) gen

-- 24
rndRange :: (Num a, Enum a) => Int -> a -> IO [a]
rndRange n m = rndSelect [1..m] n

-- 25
rndPermu :: [a] -> IO [a]
rndPermu []     = return []
rndPermu (x:xs) = do
    rand <- randomRIO (0, length xs)
    rest <- rndPermu xs
    let (ys,zs) = splitAt rand rest
    return $ ys ++ (x:zs)

-- 26
combinations :: Eq a => Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations n xs = [(xs !! i) : ys | i <- [0..length xs - 1], ys <- combinations (n - 1) (drop (i + 1) xs)]

-- 27
groups :: Eq a => [Int] -> [a] -> [[[a]]]
groups nns@(n:ns) xs
    | sum nns /= length xs = error "groups: sum of sizes is not equal to total"
    | otherwise = [xs' : yss | xs' <- combinations n xs, yss <- groups ns (xs \\ xs')]
groups _ _ = [[]]

-- 28
lengthSort :: [[a]] -> [[a]]
lengthSort = sortBy (compare `on` length)

lengthFrequencySort :: [[a]] -> [[a]]
lengthFrequencySort = concat .lengthSort . groupBy ((==) `on` length) . lengthSort
