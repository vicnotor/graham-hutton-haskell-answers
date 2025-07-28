-- 7.1
-- [f x | x <- xs, p x]
-- =
-- map f (filter p xs)
-- =
-- (map f . filter p)

-- 7.2
-- a
all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' p (b : bs) = p b && all' p bs

-- or
-- all' p = and . map p

-- b
any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = True
any' p (b : bs) = p b && all' p bs

-- or
-- any' p = or . map p

-- c
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x : xs)
  | p x = x : takeWhile' p xs
  | otherwise = []

-- d
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x : xs)
  | p x = dropWhile' p xs
  | otherwise = x : xs

-- 7.3
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

--
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x : xs else xs) []

-- 7.4
dec2int :: [Int] -> Int
dec2int = foldl (\y x -> x + 10 * y) 0

-- 7.5
curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f = \x y -> f (x, y)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f = \(x, y) -> f x y

-- 7.6
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

type Bit = Int

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold null (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (const False) f f

-- 7.9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g [x] = [f x]
altMap f g (x : y : xs) = f x : g y : altMap f g xs

-- 7.10
luhnDouble :: Int -> Int
luhnDouble x
  | y > 9 = y - 9
  | otherwise = y
  where
    y = x * 2

luhn :: [Int] -> Bool
luhn xs
  | even $ length xs = sum (altMap luhnDouble id xs) `mod` 10 == 0
  | otherwise = sum (altMap id luhnDouble xs) `mod` 10 == 0
