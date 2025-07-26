-- 5.1
sumSquares = sum [x ^ 2 | x <- [1 .. 100]]

-- 5.2
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0 .. m], y <- [0 .. n]]

-- 5.3
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

-- 5.4
replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [0 .. n]]

-- 5.5
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], x ^ 2 + y ^ 2 == z ^ 2]

-- 5.6
factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n], sum (drop 1 $ reverse $ factors x) == x]

-- or
-- perfects n = [x | x <- [1 .. n], sum (init factors x) == x]

-- 5.7
twoGenerators = [(x, y) | x <- [1, 2], y <- [3, 4]] -- [(1,3),(1,4),(2,3),(2,4)]

twoComprehensions = concat [[(x, y) | y <- [3, 4]] | x <- [1, 2]]

-- 5.8
find :: (Eq a) => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = [p | (p, x') <- zip [0 ..] xs, x == x']

-- 5.9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]

-- 5.10
-- add an isUpper guard to `shift`
-- add an `uppers` function besides the `lowers` function
-- change `freqs` to
-- freqs xs = [percent (count x xs) n | x <- ['a'..'z'] ++ ['A'..'Z']
--            where n = lowers xs ++ uppers xs
-- add the common capital letter frequencies to `table`
--
-- a more elegant solution would consider the same letter frequencies for
-- uppercase letters.
