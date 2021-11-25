module Day3
    ( run,
    ) where

data Square = Open | Tree
    deriving (Show)

squareFromChar '#' = Tree
squareFromChar _ = Open

type Grid = [[Square]]

getSquare :: Int -> [Square] -> Square
getSquare x g = g !! (x `mod` length g)

countSquare x g =
    case getSquare x g of
    Tree -> 1
    _ -> 0

parseGrid :: [String] -> Grid
parseGrid = map $ map squareFromChar


part1 :: Grid -> Int
part1 = trav 0
    where
        trav _ [] = 0
        trav x (g:gs) = countSquare x g + trav (x + 3) gs

part2 :: Grid -> Int
part2 g = 
    product $ map (\f -> f 0 g) [trav 1 1,  trav 3 1, trav 5 1, trav 7 1, trav 1 2]
    where 
        trav _ _ _ [] = 0
        trav xx yy x (g:gs) = countSquare x g + trav xx yy (x + xx) (drop (yy-1) gs)

run :: IO ()
run = do
    input <- lines <$> readFile "data/day3/input.txt"
    print $ part1 $ parseGrid input
    print $ part2 $ parseGrid input
