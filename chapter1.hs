-- 1.1
-- double (double 2)
-- = {apply inner double}
-- double (2 + 2)
-- = {apply outer double}
-- (2 + 2) + (2 + 2)
-- = {apply inner +'s}
-- 4 + 4
-- = {apply +}
-- 8

-- 1.2
-- sum [x]
-- =
-- sum (x:[])
-- =
-- x + sum []
-- =
-- x + 0
-- =
-- x

-- 1.3
product' :: (Num a) => [a] -> a
product' [] = 1
product' (n : ns) = n * product' ns

-- 1.4
-- smaller and larger should be swapped

-- 1.5
-- This would remove duplicate values
