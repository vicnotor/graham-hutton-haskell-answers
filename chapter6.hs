-- 6.1
-- It would continue infinitely with argument (-1)
-- Safer version:
fac :: Int -> Int
fac 0 = 1
fac n
  | n > 0 = n * fac (n - 1)
  | otherwise = 1

-- 6.2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n
  | n > 0 = n + sumdown (n - 1)
  | otherwise = 0

-- 6.3
(^^^) :: Int -> Int -> Int
m ^^^ 0 = 1
m ^^^ n = m * (m ^^^ (n - 1))

-- 2 ^^^ 3
-- = { apply ^^^ }
-- 2 * (2 ^^^ 2)
-- = { apply ^^^ }
-- 2 * (2 * (2 ^^^ 1)))
-- = { apply ^^^ }
-- 2 * (2 * (2 * (2 ^^^ 0))))
-- = { apply ^^^ }
-- 2 * (2 * (2 * 1)))
-- { apply * }
-- 8

-- 6.4
euclid :: Int -> Int -> Int
euclid x y
  | x == y = x
  | x < y = euclid x (y - x)
  | x > y = euclid (x - y) y

-- 6.5
-- length [1,2,3]
-- = { apply length }
-- 1 + length [2,3]
-- = { apply length }
-- 1 + 2 + length [3]
-- = { apply length }
-- 1 + 2 + 3 + length []
-- = { apply length }
-- 1 + 2 + 3 + 0
-- { apply + }
-- 6

-- drop 3 [1,2,3,4,5]
-- { apply drop }
-- drop 2 [2,3,4,5]
-- { apply drop }
-- drop 1 [3,4,5]
-- { apply drop }
-- drop 0 [4,5]
-- { apply drop }
-- [4,5]

-- init [1,2,3]
-- { apply init }
-- 1 : init [2,3]
-- { apply init }
-- 1 : 2 : init [3]
-- { apply init }
-- 1 : 2 : []
-- { list notation }
-- [1,2]

-- 6.6
-- a
and' :: [Bool] -> Bool
and' [] = True
and' (b : bs) = b && and' bs

-- b
concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs : xss) = xs ++ concat' xss

-- c
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x

-- d
(!!!) :: [a] -> Int -> a
[] !!! _ = error "(!!!): index out of bound."
(x : _) !!! 0 = x
(x : xs) !!! n = xs !!! (n - 1)

-- e
elem' :: (Eq a) => a -> [a] -> Bool
elem' e [] = False
elem' e (x : xs) = e == x || elem' e xs

-- 6.7
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x <= y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

-- 6.8
halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve xs = (take n xs, drop n xs)
  where
    n = length xs `div` 2

msort :: (Ord a) => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort h1) (msort h2)
  where
    (h1, h2) = halve xs

-- 6.9
-- a
-- sum' :: (Num a) => [a] -> a
-- sum' [] = 0
-- sum' (x : xs) = x + sum' xs
-- which becomes
sum' :: (Num a) => [a] -> a
sum' = foldr (+) 0

-- b
take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x : xs) = x : take' (n - 1) xs

-- c
last' :: [a] -> a
last' [] = error "last': tried to get last element of empty list"
last' [x] = x
last' (x : xs) = last' xs
