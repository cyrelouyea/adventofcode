module Main where

import System.IO

data Slope = Slope { right::Int, down::Int }

main :: IO ()
main = do
  m <- doReadFile
  let slopes = [Slope 1 1, Slope 3 1, Slope 5 1, Slope 7 1, Slope 1 2]
  print(foldl (\v slope -> v * (doTakeToboggan m 0 slope)) 1 slopes)

doTakeToboggan :: [[Char]] -> Int -> Slope -> Int
doTakeToboggan m x (Slope right down) = do
  let m1 = goDown m down
  if null m1 
    then 0
  else do
    let newX = rem (x + right) (length (head m1))
    let c = head m1 !! newX
    if c == '#' 
      then 1 + doTakeToboggan m1 newX (Slope right down)
      else doTakeToboggan m1 newX (Slope right down)

goDown :: [[Char]] -> Int -> [[Char]]
goDown m y
  | null m || y == 0 = m
  | otherwise = goDown (tail m) (y - 1)

doReadFile :: IO [[Char]]
doReadFile = do
  withFile "input" ReadMode doReadLine

doReadLine :: Handle -> IO [[Char]]
doReadLine hFile = do
  isEnd <- hIsEOF hFile
  if isEnd
    then return []
    else do
      line <- hGetLine hFile
      rest <- doReadLine hFile
      return (line : rest)
