-- https://wiki.haskell.org/99_questions/1_to_10

-- 1
myLast :: [a] -> a
myLast [] = error "No last element in an empty list!"
myLast [x] = x
myLast (_:xs) = myLast xs

-- 2
myButLast :: [a] -> a
myButLast [x,y]    = x
myButLast (x:y:ys) = myButLast (y:ys)
myButLast _        = error "No last but one element in an empty or singleton list!"

-- 3
elementAt :: [a] -> Int -> a
elementAt (x:_)  1 = x
elementAt (x:xs) n = elementAt xs (n - 1)

-- 4
myLength :: Num b => [a] -> b
myLength = foldl (\acc _ -> acc + 1) 0

-- 5
myReverse :: [a] -> [a]
myReverse = foldl (\xs x -> x : xs) []

-- 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == myReverse xs

-- 7
data NestedList a = Elem a | List [NestedList a] deriving (Show)
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List ls) = ls >>= flatten

-- 8
compress :: Eq a => [a] -> [a]
compress []     = []
compress [x]    = [x]
compress (x:y:ys)
    | x == y    = compress (y:ys)
    | otherwise = x : compress (y:ys)
    
-- 9
pack :: Eq a => [a] -> [[a]]
pack (x:xs) = (x : pre) : pack suf
    where (pre,suf) = span (== x) xs
pack _ = []

-- 10
encode :: Eq a => [a] -> [(Int,a)]
encode = map (\xs -> (length xs, head xs)) . pack
