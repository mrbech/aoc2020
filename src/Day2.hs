module Day2
    ( run,
    ) where
import Data.List.Split
import Data.String.Trim

data Policy = Policy {
    char :: Char,
    lowest :: Int,
    highest :: Int
} deriving (Show)

parsePolicy s =
    Policy { char = char, lowest = lowest, highest = highest }
    where 
        split = splitOn " " s
        splitMinMax = splitOn "-" $ head split
        char = head $ last split 
        lowest = read $ head splitMinMax
        highest = read $ last splitMinMax

parsePolicyPasswordLine s =
    (policy, password)
    where 
        split = splitOn ":" s
        password = trim $ last split
        policy = parsePolicy $ trim $ head split

countChar s c = length $ filter (== c) s

checkPolicy (policy, password) = 
    count >= lowest policy && count <= highest policy
    where 
        count = countChar password (char policy) 

part1 = length . filter checkPolicy

checkPolicy2 (policy, password) = 
    case (pos1, pos2) of
        (True, True) -> False
        (False, False) -> False
        _ -> True
    where
        pos1 = (== char policy)  $ password !! (lowest policy - 1)
        pos2 = (== char policy)  $ password !! (highest policy - 1)

part2 = length . filter checkPolicy2

run :: IO ()
run = do
    input <- readFile "data/day2/input.txt"
    let parsedLines = map parsePolicyPasswordLine $ lines input
    print $ part1 parsedLines
    print $ part2 parsedLines
