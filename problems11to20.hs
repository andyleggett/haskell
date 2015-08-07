module ProblemsElevenToTwenty (
	encodeModified,
	decodeModified,
	Encode(..),
	dupli,
	dupli',
	dropEvery,
	dropEvery',
	split,
	slice,
	slice',
	rotate,
	removeAt,
	removeAt'
	)
	where

import ProblemsOneToTen

--using a new Encode type to store encoded string differently
--two value constructors are defined on for a Single value and one for 
--Multiple values
data Encode a = Single a | Multiple Int a
	deriving (Show)

--using the new type in a modified encode
encodeModified :: (Eq a) => [a] -> [Encode a]
encodeModified l = map countGroups $ pack l
	where countGroups g
		| length g == 1 = Single (head g)
		| otherwise 	= Multiple (length g) (head g)

decodeModified :: [Encode a] -> [a]
decodeModified l = foldr decodeItem [] l
	where 
		decodeItem (Single x) acc = x : acc
		decodeItem (Multiple c x) acc = replicate c x ++ acc


--encodeDirect :: [a] -> [Encode a]
--encodeDirect l = foldl encodeItem

dupli :: [a] -> [a]
dupli = foldr (\x acc -> x : x : acc) []

dupli' :: [a] -> Int -> [a]
dupli' l n = foldr (\x acc -> replicate n x ++ acc) [] l

dropEvery :: [a] -> Int -> [a]
dropEvery l n = [x | (x, i) <- zip l [1..], i `mod` n > 0]

dropEvery' :: [a] -> Int -> [a]
dropEvery' l n = foldr (\x acc -> if snd x `mod` n == 0 then acc else fst x : acc) [] (zip l [1..])

split :: [a] -> Int -> ([a], [a])
split l n = ([x | (x, i) <- indexedl, i <= n], [x | (x, i) <- indexedl, i > n])
	where indexedl = zip l [1..]

slice :: [a] -> Int -> Int -> [a]
slice l n m = foldr (\x acc -> if snd x >= n && snd x <= m then fst x : acc else acc) [] (zip l [1..])

slice' :: [a] -> Int -> Int -> [a]
slice' l n m = [x | (x, i) <- zip l [1..], i >= n && i <= m]

rotate :: [a] -> Int -> [a]
rotate l d = foldl (\acc x -> acc ++ [l !! ((snd x + d - 1) `mod` length l)]) [] (zip l [1..])

removeAt :: [a] -> Int -> [a]
removeAt l p = [x | (x, i) <- zip l [1..], i /= p]

removeAt' :: [a] -> Int -> [a]
removeAt' l p = take (p-1) l ++ drop (p+1) l


