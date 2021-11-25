module Day5
    ( run,
    ) where
import Data.List (sort)

computeRow :: Int -> Int -> String -> Int
computeRow lower _ ['F'] = lower
computeRow _ upper ['B'] = upper
computeRow lower upper ('F':ss) = computeRow lower ((lower + upper) `div` 2) ss
computeRow lower upper ('B':ss) = computeRow (((lower + upper) `div` 2)+1) upper ss

computeColumn :: Int -> Int -> String -> Int
computeColumn lower _ ['L'] = lower
computeColumn _ upper ['R'] = upper
computeColumn lower upper ('L':ss) = computeColumn lower ((lower + upper) `div` 2) ss
computeColumn lower upper ('R':ss) = computeColumn (((lower + upper) `div` 2)+1) upper ss

seatID :: String -> Int
seatID s = row * 8 + column
    where
        row = computeRow 0 127 $ take 7 s
        column = computeColumn 0 7 $ drop 7 s

part1 :: [String] -> Int
part1 = maximum . map seatID

part2 = gap . sort . map seatID
    where
        gap (x:y:xs)
            | y - x == 2 = x + 1
            | otherwise = gap (y:xs)
        gap _ = -1

run :: IO ()
run = do
    input <- lines <$> readFile "data/day5/input.txt"
    print $ part1 input
    print $ part2 input
