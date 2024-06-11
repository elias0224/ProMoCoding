
--A9-1


data  BinTree a = Empty | Node a (BinTree a) (BinTree a) deriving Show

leaf :: a -> BinTree a
leaf x = Node x Empty Empty

--a)
a1 = Node 6 (Node 1 Empty (Node 3 Empty (leaf 5))) (Node 8 Empty (Node 12 (leaf 9) Empty))

--b)

istBST :: Ord a => BinTree a -> Bool
istBST Empty = False
istBST (Node x xs ys) = checktree (<=x) xs && checktree (>x) ys where
    checktree :: Ord a => (a -> Bool) -> BinTree a -> Bool
    checktree f (Node x xs ys) = (f x) && checktree f xs && checktree f ys

--c)
depth :: (Num a, Ord a) => BinTree t -> a
depth Empty          = 0
depth (Node x xs ys) = 1 + maximum(depth xs, depth ys)

--d)
insert :: Ord a => a -> BinTree a -> BinTree a
insert x Empty                      = leaf x
insert x (Node x' xs ys) | x' >= x  = Node x' (insert x xs) ys 
                         |otherwise = Node x' xs (insert x ys)

--e)
elem' :: Ord a => a -> BinTree a -> Bool
elem' x Empty                      = False
elem' x (Node x' xs ys) | x' >= x  = x==x' || elem' x xs
                        |otherwise = x==x' || elem' x ys

--f) 

leaves :: Num a => BinTree a -> a
leaves Empty                = 0
leaves (Node x Empty Empty) = 1
leaves (Node x xs ys)       = leaves xs + leaves ys

--A9-2

--a)

data Term b = Const b | Mult (Term b) (Term b) | Div (Term b) (Term b) | Add (Term b) (Term b) | Sub (Term b) (Term b) | Neg (Term b)

a2 = Mult(Add (Const 5) (Const 4)) (Sub (Const 3) (Const 2))

--b) und c)
eval' :: Integral b => Term b -> b
eval' (Const b)  = b
eval' (Sub x y)  = eval' x - eval' y
eval' (Add x y)  = eval' x + eval' y
eval' (Mult x y) = eval' x * eval' y
eval' (Div x y)  = div (eval' x) (eval' y)
eval' (Neg x)    = - (eval' x)

--d)
simplify :: Term a -> Term a
simplify (Neg (Neg x))     = x
simplify (Add x (Neg y))   = Sub x y
simplify (Sub x (Neg y))   = Add x y
simplify (Add (Neg x) y)   = Sub y x 



