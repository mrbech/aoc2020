module Day1
    ( run,
    ) where

import qualified Data.List as List
import qualified Data.Set as Set

part1 :: [Integer] -> Integer
part1 =
  product . match . Set.fromList
  where
    match numbers = Set.filter (\x -> Set.member (2020 - x) numbers) numbers

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- List.tails l, y <- ys]

part2 input =
    x1 * x2 * x3
    where
        numbers = Set.fromList input
        (x1, x2) = List.head $ List.filter (\(x,y) -> Set.member (2020 - x - y) numbers) (pairs input)
        x3 = 2020 - x1 - x2

run :: IO ()
run = do
    input <- readFile "data/day1/input.txt"
    let numbers = ((map read $ words input) :: [Integer])
    print "Part 1:"
    print $ part1 numbers
    print "Part 2:"
    print $ part2 numbers
