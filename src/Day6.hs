module Day6
  ( run,
  )
where
import Data.List

groupLines :: [String] -> [[String]]
groupLines = gl []
    where
        gl rs ("":xs) = gl ([]:rs) xs
        gl (r:rs) (x:xs) = gl ((x:r):rs) xs
        gl [] (x:xs) = gl [[x]] xs
        gl rs [] = rs

part1 :: [[String]] -> Int
part1 = sum . map (length . nub . concat)

part2 :: [[String]] -> Int
part2 = sum . map countAllYes
    where countAllYes g = length $ filter (\a -> length a == length g) (group . sort $ concat g)

run :: IO ()
run = do
    input <- lines <$> readFile "data/day6/input.txt"
    print $ part1 $ groupLines input
    print $ part2 $ groupLines input
