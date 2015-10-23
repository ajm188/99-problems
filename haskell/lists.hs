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
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- # 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

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
