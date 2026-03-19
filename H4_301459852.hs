-- Lauren Yip
-- 301459852 CMPT383

-- The ErrJst type
data ErrJst e j = Err e | Jst j deriving (Show)

-- Question 1: Functor
instance Functor (ErrJst e) where
    fmap :: (a -> b) -> ErrJst e a -> ErrJst e b
    fmap f (Jst j) = Jst (f j)
    fmap _ (Err e) = Err e

-- Question 2: Applicative
instance Applicative (ErrJst e) where
    pure :: a -> ErrJst e a
    pure x = Jst x

    (<*>) :: ErrJst e (a -> b) -> ErrJst e a -> ErrJst e b
    (<*>) (Jst f) (Jst j) = Jst (f j)
    (<*>) (Err e) _       = Err e
    (<*>) _       (Err e) = Err e

-- Question 3: Monad
instance Monad (ErrJst e) where
    return :: a -> ErrJst e a
    return = pure

    (>>=) :: ErrJst e a -> (a -> ErrJst e b) -> ErrJst e b
    (>>=) (Jst j) f = f j
    (>>=) (Err e) _ = Err e

-- Question 4: Join
join :: Monad m => m (m a) -> m a
join mma = mma >>= id

-- Question 5: LTree and Foldable
data LTree a = Leaf a | LNode (LTree a) (LTree a) deriving (Show)

instance Foldable LTree where
    foldr :: (a -> b -> b) -> b -> LTree a -> b
    foldr f acc (Leaf x)    = f x acc
    foldr f acc (LNode l r) =
        let rightResult = foldr f acc r
            leftResult  = foldr f rightResult l
        in leftResult