import Data.Functor

--using newtype keyword to fmap over first element of a tuple
--the newtype allows only one value constructor
newtype Pair b a = Pair (a,b)

--new type Pair b a = Pair { getPair :: (a,b)}
--record syntax provides the getPair function (unboxing)

--the type constructor Pair c expects one more type
-- fmap :: (a -> b) -> Pair c a -> Pair c b
instance Functor (Pair c) where
	fmap f (Pair (x,y)) = Pair (f x, y)

--pattern matching inside Pair
getPair :: Pair b a -> (a,b)
getPair (Pair (a, b)) = (a, b)