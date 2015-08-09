module ProblemsTwentyOneToThirty (
		insertAt,
		range,
		range',
		randomSelect,
		drawNFromM
	)
	where

import System.Random

insertAt :: [a] -> [a] -> Int -> [a]
insertAt x y p = take (p-1) y ++ x ++ drop (p+1) y


range :: Int -> Int -> [Int]
range x y = [x .. y]

range' :: Int -> Int -> [Int]
range' x y = drop (x-1) . take y  $ [1..]


drawNFromM :: Int -> Int -> Int -> [Int]
drawNFromM n m s = take n (randomRs (1, m) (mkStdGen s))

randomSelect :: [a] -> Int -> Int -> [a]
randomSelect l n s =
		
		in 

randomFrom1ToM :: Int -> Int -> [Int]
randomFrom1ToM m s = 