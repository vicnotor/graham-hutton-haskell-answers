-- 2.2
-- (x^3)*4
-- (2*3)+(4*5)
-- 2+((3*4)^5)

-- 2.3
n = a `div` length xs
  where
    a = 10
    xs = [1, 2, 3, 4, 5]

-- 2.4
last' :: [a] -> a
last' [] = error "Tried to get last element of empy list"
last' xs = (head . reverse) xs

-- or
-- last' xs = head (reverse xs)
-- or
-- last' xs = head $ reverse xs

-- 2.5
init' :: [a] -> [a]
init' [] = []
init' xs = take (length xs - 1) xs

-- or
-- init' xs = reverse (drop 1 (reverse xs))
-- or
-- init' xs = reverse $ drop 1 $ reverse xs
