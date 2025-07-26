-- 4.1
halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve xs = (take n xs, drop n xs)
  where
    n = length xs `div` 2

-- 4.2
-- a
third :: [a] -> a
third xs
  | length xs >= 3 = head $ tail $ tail xs
  | otherwise = error "third: length of list must be at least 3"

-- b
third' :: [a] -> a
third' xs
  | length xs >= 3 = xs !! 2
  | otherwise = error "third: length of list must be at least 3"

-- c
third'' :: [a] -> a
third'' (_ : _ : x : _) = x
third'' _ = error "third: length of list must be at least 3"

-- 4.3
-- a
safetail :: [a] -> [a]
safetail xs = if null xs then xs else tail xs

-- b
safetail' :: [a] -> [a]
safetail' xs
  | null xs = xs
  | otherwise = tail xs

-- c
safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (x : xs) = xs

-- 4.4
(||) :: Bool -> Bool -> Bool
True || True = True
True || False = True
False || True = True
False || False = False

(|||) :: Bool -> Bool -> Bool
True ||| _ = True
_ ||| True = True
_ ||| _ = False

(||||) :: Bool -> Bool -> Bool
True |||| _ = True
False |||| x = x

(|||||) :: Bool -> Bool -> Bool
a ||||| b
  | a = a
  | b = b
  | otherwise = False

(||||||) :: Bool -> Bool -> Bool
False |||||| False = False
_ |||||| _ = True

(|||||||) :: Bool -> Bool -> Bool
a ||||||| b
  | a == b = a
  | otherwise = True

-- 4.5
(&&) :: Bool -> Bool -> Bool
a && b =
  if a
    then
      if b then True else False
    else False

-- 4.6
(&&&) :: Bool -> Bool -> Bool
a &&& b = if a then b else False

-- 4.7
mult :: Int -> (Int -> (Int -> Int))
mult = \x -> (\y -> (\z -> x * y * z))

-- 4.8
luhnDouble :: Int -> Int
luhnDouble x
  | y > 9 = y - 9
  | otherwise = y
  where
    y = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z w = ((luhnDouble x + y + luhnDouble z + w) `mod` 10) == 0
