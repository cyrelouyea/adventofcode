module Main where

import Data.List
import System.IO

main :: IO ()
main = do
  m <- doReadFile
  
  let (one, two, three) = countDiff (sort $ map read m :: [Int]) 0
  print (one * three)
  return ()

countDiff :: [Int] -> Int -> (Int, Int, Int)
countDiff js prev 
  | null js = (0, 0, 1)
  | otherwise = case (head js) - prev of
      1 -> addTuple (1, 0, 0) (countDiff (tail js) (head js))
      2 -> addTuple (0, 1, 0) (countDiff (tail js) (head js))
      3 -> addTuple (0, 0, 1) (countDiff (tail js) (head js))
      
addTuple :: Num a => (a, a, a) -> (a, a, a) -> (a, a, a)
addTuple (a1, b1, c1) (a2, b2, c2) = (a1+a2, b1+b2, c1+c2)

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