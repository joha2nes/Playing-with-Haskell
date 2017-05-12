
import Data.List

-- problem 1
myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs

-- problem 2
myButLast :: [a] -> a
myButLast = last . init

-- problem 3
elemAt :: [a] -> Int -> a
elemAt (x:_) 1 = x
elemAt (_:xs) i = elemAt xs (i - 1)

-- problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

isPalindrome' [] = True
isPalindrome' [_] = True
isPalindrome' xs = (head xs) == (last xs) && (isPalindrome' $ init $ tail xs)

-- problem 7
data NestedList a = Elem a | List [NestedList a]

-- problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs) = if x == y
						then compress $ x : xs
						else x : y : (compress xs)


compress' :: Eq a => [a] -> [a]
compress' = map head . group

-- problem 9
--pack :: Eq a => [a] -> [[a]]
--pack