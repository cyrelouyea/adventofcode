module Main where

import Data.List
import System.IO

main :: IO ()
main = do
  m <- doReadFile
  let (init, rest) = splitAt 25 $ map (\n -> read n :: Int) m
  case findInvalid rest init of
    Nothing -> error "No invalid number found"
    Just n -> print n
  return ()

findInvalid :: (Eq a, Num a) => [a] -> [a] -> Maybe a
findInvalid xs ls
  | null xs = Nothing
  | not $ any ((== n) . sum) (combinations 2 ls) = Just n
  | otherwise = findInvalid (tail xs) (tail ls ++ [n])
  where
    n = head xs

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

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs =
  [ y : ys | y : xs' <- tails xs, ys <- combinations (n - 1) xs'
  ]