module Data.String.Trim (trim) where
import Data.Char (isSpace)
import Data.List

trim = dropWhileEnd isSpace . dropWhile isSpace
