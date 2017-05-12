-- problem 1

p1 :: Int
p1 = sum $ filter (\n -> n `mod` 3 == 0 || n `mod` 5 == 0) [0..999]

-- problem 2

fibonacci :: [Integer]
fibonacci = fibonacci' 1 2
	where fibonacci' a b = a : fibonacci' b (a+b)

p2 = sum . filter even $ takeWhile (< 4000000) fibonacci