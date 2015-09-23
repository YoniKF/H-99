-- https://wiki.haskell.org/99_questions/11_to_20

-- 11
data Code a = Single a | Multiple Int a deriving (Show)
encodeModified :: Eq a => [a] -> [Code a]
encodeModified = map (\xxs@(x:xs) -> if null xs then Single x else Multiple (length xxs) x) . pack

-- 12
decodeModified :: [Code a] -> [a]
decodeModified = (>>= decode)
    where decode (Single x)     = [x]
          decode (Multiple n x) = replicate n x
          
-- 13
encodeDirect :: Eq a => [a] -> [Code a]
encodeDirect = foldr step []
    where step x (Single y : cs)
              | x == y = Multiple 2 y : cs
          step x acc@(Multiple n y : cs)
              | x == y = Multiple (n + 1) y : cs
          step x cs    = Single x : cs

-- 14
dupli :: [a] -> [a]
dupli = foldr (\x xs -> x:x:xs) []

-- 15
repli :: [a] -> Int -> [a]
repli xs n = foldr (\x acc -> (replicate n x) ++ acc) [] xs

-- 16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = pre ++ dropEvery (drop 1 suf) n
    where (pre, suf) = splitAt (n - 1) xs
    
-- 17
myTake, myDrop :: Int -> [a] -> [a]
myTake n (x:xs)
    | n > 0 = x : myTake (n - 1) xs
myTake _ _  = []
myDrop n (_:xs)
    | n > 0 = myDrop (n - 1) xs
myDrop _ xs = xs
          
split :: [a] -> Int -> ([a],[a])
split xs n = (myTake n xs, myDrop n xs)

-- 18
slice :: [a] -> Int -> Int -> [a]
slice xs m n = myDrop (m - 1) $ myTake n xs

-- 19
rotate :: [a] -> Int -> [a]
rotate xs n
    | n >= 0    = drop m xs ++ take m xs
    | otherwise = rotate xs (len + n)
    where len = length xs
          m = n `mod` len

-- 20
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs
    | n < 1 || n > length xs = error "removeAt: index out of bounds"
removeAt n xs = (last pre, init pre ++ suf)
    where (pre, suf) = splitAt n xs
