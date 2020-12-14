module Main where

import System.IO

data Password = Password
  { min :: Int,
    max :: Int,
    letter :: Char,
    pwd :: [Char]
  }

isValid :: Password -> Bool
isValid (Password min max letter pwd) = do
  let count = countLetterInStr pwd letter
  (min <= count) && (count <= max)

doReadFile :: IO [Password]
doReadFile = do
  withFile "input" ReadMode doReadLine

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
parseLine line = do
  let (min, r1) = takeMin line
  let (max, r2) = takeMax r1
  let (letter, pwd) = takeLetter r2
  Password min max letter pwd

takeMin :: [Char] -> (Int, [Char])
takeMin line = do
  let (min, rest) = splitFirst line '-'
  (read min, rest)

takeMax :: [Char] -> (Int, [Char])
takeMax line = do
  let (max, rest) = splitFirst line ' '
  (read max, rest)

takeLetter :: [Char] -> (Char, [Char])
takeLetter line = do
  let (letter, rest) = splitFirst line ':'
  (head letter, dropWhile (==' ') rest)

splitFirst :: [Char] -> Char -> ([Char], [Char])
splitFirst s l 
  | null s = ("", "")
  | head s == l = ("", tail s)
  | otherwise = do
      let (first, last) = splitFirst (tail s) l
      (head s : first, last)

countLetterInStr :: [Char] -> Char -> Int
countLetterInStr s l
  | null s = 0
  | head s == l = 1 + countLetterInStr (tail s) l
  | otherwise = countLetterInStr (tail s) l
    
main = do
  passwords <- doReadFile
  print(length (filter isValid passwords))