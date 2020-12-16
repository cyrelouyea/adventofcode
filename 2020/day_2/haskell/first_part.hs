module Main where

import System.IO

data Password = Password
  { min :: Int,
    max :: Int,
    letter :: Char,
    pwd :: [Char]
  }

main = do
  passwords <- doReadFile
  print (length (filter isValid passwords))
  return ()

isValid :: Password -> Bool
isValid (Password min max letter pwd) =
  (min <= count) && (count <= max)
  where
    count = countLetterInStr pwd letter

doReadFile :: IO [Password]
doReadFile = withFile "input" ReadMode doReadLine

doReadLine :: Handle -> IO [Password]
doReadLine hFile = do
  isEnd <- hIsEOF hFile
  if isEnd
    then return []
    else do
      line <- hGetLine hFile
      rest <- doReadLine hFile
      return (parseLine line : rest)

parseLine :: [Char] -> Password
parseLine line = Password min max letter pwd
  where
    (min, r1) = takeFirst line
    (max, r2) = takeSecond r1
    (letter, pwd) = takeLetter r2

takeFirst :: [Char] -> (Int, [Char])
takeFirst line = (read min, rest)
  where
    (min, rest) = splitFirst line '-'

takeSecond :: [Char] -> (Int, [Char])
takeSecond line = (read max, rest)
  where
    (max, rest) = splitFirst line ' '

takeLetter :: [Char] -> (Char, [Char])
takeLetter line = (head letter, dropWhile (== ' ') rest)
  where
    (letter, rest) = splitFirst line ':'

splitFirst :: [Char] -> Char -> ([Char], [Char])
splitFirst s l
  | null s = ("", "")
  | head s == l = ("", tail s)
  | otherwise = (head s : first, last)
  where
    (first, last) = splitFirst (tail s) l

countLetterInStr :: [Char] -> Char -> Int
countLetterInStr s l
  | null s = 0
  | head s == l = 1 + countLetterInStr (tail s) l
  | otherwise = countLetterInStr (tail s) l