myLast :: [a] -> a
myLast (x:[]) = x
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast (x:x':[]) = x
myButLast (_:_:xs) = myButLast xs

elementAt :: [a] -> Int -> a
elementAt (x:xs) k
    | k == 0 = x
    | otherwise = elementAt xs $ k - 1

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome a = all (\(a, b) -> a == b) $ zip a (myReverse a)

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = (flatten x) ++ (flatten (List xs))

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:[]) = [x]
compress (x:l@(x':xs))
    | x == x' = compress l
    | otherwise = x:compress l

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:[]) = [[x]]
pack (x:l@(x':xs))
    | x == x' = let (y:ys) = pack l in (x:y):ys
    | otherwise = [x]:pack l

encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\l@(y:ys) -> ((myLength l), y)) $ pack xs