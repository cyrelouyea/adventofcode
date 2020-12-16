module Main where

import System.IO

bitSize = 10

main :: IO ()
main = do
  m <- doReadFile
  print $ maxIn $ map decodeSeat m
  return ()

maxIn :: Ord b => [b] -> b
maxIn l = foldl max (head l) l

decodeChar :: Char -> Int
decodeChar c 
  | c == 'F' = 0
  | c == 'B' = 1
  | c == 'L' = 0
  | c == 'R' = 1
  | otherwise = error ("Invalid char: " ++ [c])

decodeSeat :: [Char] -> Int
decodeSeat seat = do
  sum $ 
    zipWith  
      (\i b -> b * 2 ^ (bitSize - i - 1)) [0..]
      (map decodeChar seat)

doReadFile :: IO [[Char]]
doReadFile = withFile "input" ReadMode doReadLine

doReadLine :: Handle -> IO [[Char]]
doReadLine hFile = do
  isEnd <- hIsEOF hFile
  if isEnd
    then return []
    else do
      line <- hGetLine hFile
      rest <- doReadLine hFile
      return (line : rest)
