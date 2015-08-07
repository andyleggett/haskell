module ProblemsOneToTen (
	lastInList,
	lastButOneInList,
	elementAt,
	lengthOfList,
	lengthOfList',
	reverseTheList,
	reverseTheList',
	reverseTheList'',
	reverseTheList''',
	isPalindrome,
	flatten,
	compress,
	compress',
	pack,
	encode,
	encode'
	)
	where

--recursive definition that takes a list of type a to a single item
--of type a.  Recursive definition matching base case of single item
--list and recursive case using the tail of the list (xs)
lastInList :: [a] -> a
lastInList [x] = x
lastInList (x:xs) = lastInList xs

--similar recursion to lastInList but pattern matches two element list
--as base case ([x,y]). Could use x:y:[] instead of sugar.
lastButOneInList :: [a] -> a
lastButOneInList [x,y] = x
lastButOneInList (x:xs) = lastButOneInList xs


--takes a list of type a and and integer position and returns an element
--of type a.  First pattern match is for empty list with _ matching any
--position.  Second pattern is base case when position is 1 and the recursive
--pattern with a guard against positions <= 0
elementAt :: [a] -> Int -> a
elementAt [] _ = undefined
elementAt (x:xs) 1 = x
elementAt (x:xs) pos 
	| pos > 1 = elementAt xs (pos - 1)
	| otherwise = undefined



--takes a list of type a to an integer.  Two definitions showing
--first a recursive definition and then using a left fold and 
--a pointfree style
lengthOfList :: [a] -> Int
lengthOfList [] = 0
lengthOfList (x:xs) = 1 + lengthOfList xs

lengthOfList' :: [a] -> Int
lengthOfList' = foldl (\acc x -> acc + 1) 0


--first reverse uses ++ concat operator and is a bit slow
reverseTheList :: [a] -> [a]
reverseTheList [] = []
reverseTheList (x:xs) = (reverseTheList xs) ++ [x]

-- this reverse uses a helper function which takes a list and a type a
--and recursively calls with the tail of a the list and an 'accumulator'
--that cons the head of the list each time.  The recusion is started with the empty list
--as a base case and 'returns' when we get to the empty list
reverseTheList' :: [a] -> [a]
reverseTheList' list = reverseSub list []
	where	
		reverseSub [] 		a = a
		reverseSub (x:xs) 	a = reverseSub xs (x:a)

--this version uses a left fold - the return list is stored
--in the accumulator - it's also pointfree

reverseTheList'' :: [a] -> [a]
reverseTheList'' = foldl (\acc x -> x : acc) []

--or noticing that the lambda above is just cons with order flipped so...

reverseTheList''' :: [a] -> [a]
reverseTheList''' = foldl (flip (:)) []


--a palindrome is a list that is the same in reverse
--so we use any of the reverseTheList functions to check for this
--the type a must be in the class Eq so that the == equality can be used
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list = reverseTheList list == list


--a recursive data type a NestedList can be one element of type a or
--a list of nested lists
data NestedList a = Elem a | List [NestedList a]

--this takes a nestedlist of type a and returns a flattened list of type a
--base case 1 is an Elem of type a that returns a singleton list of type a
--base case 2 is a List containing the empty list
--recursion is called on head and tail of value in List constructor
--and concatenated
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (x:xs)) = flatten (x) ++ flatten (List xs)


--removes duplicate runs of items
--using recursion compares the first and second element
--if equal then ignore first and recur with tail
--if not equal cons head onto recursed tail
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs)
	| x == y 	= compress (y:xs)
	| otherwise = x : compress (y:xs)

--achieve same using a foldr that compares the head of the accumulator 
--with the next element as the fold progresses
--compressSub type declaration unnecessary here
compress' :: (Eq a) => [a] -> [a]
compress' = foldr compressSub [] 
			where
				compressSub :: (Eq a) => a -> [a] -> [a]
				compressSub x [] = x : []
				compressSub x acc = if x /= head acc then x : acc else acc


--need to come back to this one, it's not very elegant
--seems like there should be a better way????
pack :: (Eq a) => [a] -> [[a]]
pack = foldr packSub []
		where
			--packSub :: (Eq a) => a -> [[a]] -> [[a]]
			packSub x [] = [[x]]
			packSub x acc = if x == head (head acc) then [x : head acc] ++ tail acc else [x] : acc



--run length encoding using pack above
--first it creates a list of packed elements
--then it maps the countGroups function over the list
--countGroups creates a tuple pair from the length of the list
--and the head of the list
encode :: (Eq a) => [a] -> [(Int, a)]
encode l = map countGroups $ pack l
	where countGroups g = (length g, head g)

--and with a lambda instead
encode' :: (Eq a) => [a] -> [(Int, a)]
encode' l = map (\g -> (length g, head g)) $ pack l

--and finally pointfree
encode'' :: (Eq a) => [a] -> [(Int, a)]
encode'' = map (\g -> (length g, head g)) . pack 

