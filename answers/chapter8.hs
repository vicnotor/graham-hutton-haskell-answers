-- 8.1
data Nat = Zero | Succ Nat deriving (Show)

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult (Succ m) n = add n (mult m n)

-- 8.2
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show)

-- example trees
t1 :: Tree Int
t1 =
  Node
    (Node (Leaf 1) 3 (Leaf 4))
    5
    (Node (Leaf 6) 7 (Leaf 9))

occurs :: (Ord a) => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = case compare x y of
  EQ -> True
  LT -> occurs x l
  GT -> occurs x r

-- This seems more efficient than the original definition because x and y are
-- only compared once per function call, instead of potentially two times
-- (x == y, and x < y)

-- 8.3
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a) deriving (Show)

t2 :: Tree' Int
t2 =
  Node'
    (Node' (Leaf' 1) (Leaf' 2))
    ( Node'
        (Leaf' 3)
        ( Node'
            (Leaf' 4)
            ( Node'
                (Leaf' 5)
                (Leaf' 6)
            )
        )
    )

leaves :: Tree' a -> Int
leaves (Leaf' _) = 1
leaves (Node' l r) = leaves l + leaves r

balanced :: Tree' a -> Bool
balanced (Leaf' _) = True
balanced (Node' l r) = abs (leaves l - leaves r) <= 1 && balanced l && balanced r

-- 8.4
split :: [a] -> ([a], [a])
split [] = ([], [])
split xs = splitAt n xs
  where
    n = length xs `div` 2

balance :: [a] -> Tree' a
balance [] = error "balance: trying to make (balanced) tree of an empty list"
balance [x] = Leaf' x
balance xs = Node' (balance l) (balance r)
  where
    (l, r) = split xs

-- 8.5
data Expr = Val Int | Add Expr Expr deriving (Show)

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

-- 8.7
data Maybe' a = Nothing' | Just' a deriving (Show)

instance (Eq a) => Eq (Maybe' a) where
  Just' x == Just' y = x == y
  _ == _ = False

data List a = Nil | Cons a (List a)

instance (Eq a) => Eq (List a) where
  Nil == Nil = True
  Cons x xs == Cons y ys = x == y && xs == ys
  _ == _ = False

-- example lists
list1 :: List Int
list1 = Cons 1 (Cons 2 Nil)

list2 :: List Int
list2 = Cons 1 (Cons 2 Nil)

list3 :: List Int
list3 = Cons 1 (Cons 3 Nil)
