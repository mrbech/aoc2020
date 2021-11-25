module Day4
    ( run,
    ) where

import Data.List
import Data.Char (isDigit)
import Data.List.Split
import Data.String.Trim

groupLines lines = map trim (gl [] lines)
    where
        gl rs ("":xs) = gl ("":rs) xs
        gl (r:rs) (x:xs) = gl ((r ++ " " ++ x):rs) xs
        gl [] (x:xs) = gl [x] xs
        gl rs [] = rs


type Passport = [(String, String)]

parseField :: String -> (String, String)
parseField s = (head kv, last kv)
    where kv = splitOn ":" s

parsePassport :: String -> Passport
parsePassport s = map parseField $ splitOn " " s

validPassport1 :: Passport -> Bool
validPassport1 p = 7 == length (withoutCID p)
    where
        withoutCID = filter (\(k,_) -> k /= "cid")

part1 :: [Passport] -> Int
part1 = length . filter validPassport1


validField :: (String, String) -> Bool
validField ("byr", value) = v >= 1920 && v <= 2002
    where 
        v = read value
validField ("iyr", value) = v >= 2010 && v <= 2020
    where 
        v = read value
validField ("eyr", value) = v >= 2020 && v <= 2030
    where 
        v = read value
validField ("hgt", value)
    | "cm" `isInfixOf` value = v >= 150 && v <= 193
    | "in" `isInfixOf` value = v >= 59 && v <= 76
    | otherwise = False
    where 
        v = read $ takeWhile isDigit value
validField ("hcl", value) = (length value == 7) && (head value == '#') && all (\x -> isDigit x || (x >= 'a' && x <= 'f')) (drop 1 value)

validField ("ecl", value) = value `elem` ["amb","blu","brn","gry","grn","hzl","oth"]

validField ("pid", value) = length value == 9 && all isDigit value

validPassport2 :: Passport -> Bool
validPassport2 p = 7 == length (filter validField (withoutCID p))
    where
        withoutCID = filter (\(k,_) -> k /= "cid")

part2 :: [Passport] -> Int
part2 = length . filter validPassport2

run :: IO ()
run = do
    input <- lines <$> readFile "data/day4/input.txt"
    let passports = map parsePassport $ groupLines input
    print $ part1 passports
    print $ part2 passports
