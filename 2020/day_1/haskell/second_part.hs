module Main where

import Data.List
import System.IO

main :: IO ()
main = do
  numbers <- doReadFile
  print (product (head (filter ((== 2020) . sum) (combinations 3 numbers))))

doReadFile :: IO [Int]
doReadFile = do
  withFile "input" ReadMode doReadLine

doReadLine :: Handle -> IO [Int]
doReadLine hFile = do
  isEnd <- hIsEOF hFile
  if isEnd
    then return []
    else do
      line <- hGetLine hFile
      rest <- doReadLine hFile
      let number = read line :: Int
      return (number : rest)

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs']