module Main where

import System.IO

data Password = Password
  { first :: Int,
    second :: Int,
    letter :: Char,
    pwd :: [Char]
  }


isValid :: Password -> Bool
isValid (Password first second letter pwd) = 
  isLetterInPos pwd letter (first-1) /= isLetterInPos pwd letter (second-1)

isLetterInPos :: [Char] -> Char -> Int -> Bool
isLetterInPos pwd letter place
  | place == -1 = False
  | null pwd = False
  | head pwd == letter && place == 0 = True
  | otherwise = isLetterInPos (tail pwd) letter (place - 1)

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
  let (first, r1) = takeFirst line
  let (second, r2) = takeSecond r1
  let (letter, pwd) = takeLetter r2
  Password first second letter pwd

takeFirst :: [Char] -> (Int, [Char])
takeFirst line = do
  let (first, rest) = splitFirst line '-'
  (read first, rest)

takeSecond :: [Char] -> (Int, [Char])
takeSecond line = do
  let (second, rest) = splitFirst line ' '
  (read second, rest)

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