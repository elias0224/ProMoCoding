
--A11-1


--a)    

data List a = Nil | Cons a (List a) deriving (Show, Eq)

--b)
instance Functor List where
    fmap _ Nil        = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

negateFunctor :: (Functor f, Num b) => f b -> f b
negateFunctor = fmap negate

--c)
instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    Cons x xs <*> Cons y ys = Cons (x y) (xs <*> ys)

zipwith' :: (a -> b -> c) -> List a -> List b -> List c
zipwith' f xs ys = f <$> xs <*> ys


--A11-2

--a)

type Money = Int 
type Account = (Money, Money)

--b)

withdraw :: Money -> Account -> Maybe Account
withdraw x (credit, debit) | debit < credit = Nothing
                           | otherwise = Just (credit, debit)
                           where 
                            credit = credit + x

deposit :: Money -> Account -> Maybe Account
deposit x (credit, debit) = Just (credit, debit + x)

--c) 



ex1a = do 
    account <- deposit 90000000 (0,0)
    account <- withdraw 40000000 account
    account <- withdraw 10000000 account
    account <- withdraw 45000000 account
    account <- deposit 6000000 account
    accountState account


ex2a =do
    account <- deposit 20000000 (0,0)
    account <- deposit 40000000 account
    account <- withdraw 15000000 account
    account <- withdraw 25000000 account
    account <- deposit 10000000 account 
    accountState account
    

--d)


ex1b = 
     deposit 90000000 (0,0) >>= \account ->
     withdraw 40000000 account >>= \account ->
     withdraw 10000000 account >>= \account ->
     withdraw 45000000 account >>= \account ->
     deposit 6000000 account >>= \account ->
     accountState account


ex2b = 
    deposit 20000000 (0,0) >>= \account ->
    deposit 40000000 account >>= \account ->
    withdraw 15000000 account >>= \account ->
    withdraw 25000000 account >>= \account ->
    deposit 10000000 account  >>= \account ->
    accountState account

--e)

type Balance = Money 

accountState :: Account -> Maybe Balance 
accountState (credit, debit)  | credit > debit  = Nothing
                              | otherwise       = Just (debit - credit)

--A11-3

--a)

data Box a = Full a | Empty String 

--b)

instance Functor Box where
    fmap _ (Empty x) = Empty x 
    fmap f (Full x)  = Full (f x)

--c) 

instance Applicative Box where
    pure x = Full x
    _ <*> (Empty x) = Empty x
    (Empty x) <*> _ = Empty x
    (Full x) <*> (Full y) = Full (x y)

--d)
  
instance Monad Box where
    return = pure

    (Full x)  >>= k         = k x
    (Empty x) >>= _         = Empty x




