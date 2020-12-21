module Main where

import Data.List
import System.IO

main :: IO ()
main = do
  m <- doReadFile
  print $ 
    nbArrangements 
      (reverse $ sort $ map read m :: [Int])
      []
  return ()

nbArrangements :: [Int] -> [Int] -> [Int]
countDiff js memo 
  | null js = memo 
  | otherwise = case (head js) -  of
    1 -> 
      
      
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