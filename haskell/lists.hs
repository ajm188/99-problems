-- # 1
myLast :: [a] -> a
myLast (x:[]) = x
myLast (_:xs) = myLast xs

-- # 2
myButLast :: [a] -> a
myButLast (x:x':[]) = x
myButLast (_:_:xs) = myButLast xs

-- # 3
elementAt :: [a] -> Int -> a
elementAt (x:xs) k
    | k == 0 = x
    | otherwise = elementAt xs $ k - 1

-- # 4
myLength :: ([a] -> Int)
myLength = foldl (\k _ -> k + 1) 0

-- # 5
myReverse :: ([a] -> [a])
myReverse = foldl (\l x -> x:l) []

-- # 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome a = all (\(a, b) -> a == b) $ zip a (myReverse a)

-- # 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = (flatten x) ++ (flatten (List xs))

-- # 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:[]) = [x]
compress (x:l@(x':xs))
    | x == x' = compress l
    | otherwise = x:compress l

-- # 9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:[]) = [[x]]
pack (x:l@(x':xs))
    | x == x' = let (y:ys) = pack l in (x:y):ys
    | otherwise = [x]:pack l

-- # 10
encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\l@(y:ys) -> ((myLength l), y)) $ pack xs

-- # 11
data Encoded a = Single a | Multiple Int a
encodeModified :: Eq a => [a] -> [Encoded a]
encodeModified xs = map modify $ encode xs
    where modify (1, x) = Single x
          modify (n, x) = Multiple n x

-- # 12
decodeModified :: [Encoded a] -> [a]
decodeModified [] = []
decodeModified ((Single x):xs) = x:decodeModified xs
decodeModified ((Multiple k x):xs) = (unpack x k) ++ (decodeModified xs)
    where unpack x k = foldl (\l _ -> x:l) [] [1..k]

-- # 13
encodeDirect :: Eq a => [a] -> [Encoded a]
encodeDirect xs = foldr encode' [] xs
    where encode' x [] = [Single x]
          encode' x l@((Single x'):xs) = if x == x' then (Multiple 2 x):xs else (Single x):l
          encode' x l@((Multiple k x'):xs) = if x == x' then (Multiple (k + 1) x):xs else (Single x):l

-- # 14
dupli :: ([a] -> [a])
dupli = foldr (\x l -> x:x:l) []

-- # 15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli xs k = foldl (\l x -> l ++ (foldl (\l _ -> x:l) [] [1..k])) [] xs

-- # 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = [x | (x, i) <- (zip xs $ cycle [1..n]), i /= n]

-- # 17
split :: [a] -> Int -> ([a], [a])
split xs k = foldr (\(x, i) (l, r) -> if i < k then (x:l, r) else (l, x:r)) ([], []) $ zip xs [0..]

-- # 18
slice :: [a] -> Int -> Int -> [a]
slice xs i k = foldr (\(x, j) l -> if j >= i then x:l else l) [] $ zip xs [1..k-1]

-- # 19
rotate :: [a] -> Int -> [a]
rotate xs k = foldl (\l _ -> rotateOnce l) xs [1..kPos]
    where kPos = if k < 0 then (length xs) + k else k
          rotateOnce (x:xs) = xs ++ [x]

-- # 20
removeAt :: Int -> [a] -> (a, [a])
removeAt k xs =
    case foldr (\(x, i) (x', l) -> if i == k then (Just x, l) else (x', x:l)) (Nothing, []) $ zip xs [1..] of
        (Just x', xs') -> (x', xs')
