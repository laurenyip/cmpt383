--Lauren Yip CMPT383
--HW3  
--301459852

-- Problem 1: List type and listZip
data List a = Empty | Cons a (List a) deriving (Show)

listZip :: List a -> List b -> List (a, b)
listZip Empty _ = Empty
listZip _ Empty = Empty
listZip (Cons x xs) (Cons y ys) = Cons (x, y) (listZip xs ys)

-- Problem 2: Binary Search Tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

insert :: Ord a => a -> Tree a -> Tree a
insert v EmptyTree = Node v EmptyTree EmptyTree
insert v (Node x left right)
    | v < x     = Node x (insert v left) right
    | otherwise = Node x left (insert v right)

-- Problem 3: Natural numbers
data Nat = Zero | Succ Nat deriving (Show)

natPlus :: Nat -> Nat -> Nat
natPlus Zero n = n
natPlus (Succ m) n = Succ (natPlus m n)

natMult :: Nat -> Nat -> Nat
natMult Zero _ = Zero
natMult (Succ m) n = natPlus n (natMult m n)

-- Problem 4: Tree Eq instance
instance Eq a => Eq (Tree a) where
    (==) EmptyTree EmptyTree = True
    (==) (Node x1 left1 right1) (Node x2 left2 right2) = 
        x1 == x2 && left1 == left2 && right1 == right2
    (==) _ _ = False

-- Problem 5: AssocList as Functor
data AssocList k v = ALEmpty | ALCons k v (AssocList k v) deriving (Show)

instance Functor (AssocList k) where
    fmap :: (a -> b) -> AssocList k a -> AssocList k b
    fmap _ ALEmpty = ALEmpty
    fmap f (ALCons key val rest) = ALCons key (f val) (fmap f rest)