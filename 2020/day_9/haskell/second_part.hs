module Main where

import Data.Array
import Data.List
import System.IO

main :: IO ()
main = do
  m <- doReadFile
  let all = map (\n -> read n :: Int) m
  let (init, rest) = splitAt 25 all
  case findInvalid rest init of
    Nothing -> error "No invalid number found"
    Just n -> case uncurry (findContiguous n) (splitAt 2 all) of
      Nothing -> error "No contiguous set found"
      Just l -> print $ maximum l + minimum l
  return ()

findContiguous :: (Num a, Ord a) => a -> [a] -> [a] -> Maybe [a]
findContiguous v ls ns
  | sum ls == v = Just ls
  | sum ls < v = findContiguous v (ls ++ [head ns]) (tail ns)
  | otherwise = findContiguous v nls (rest ++ ns)
  where
    (nls, rest) = splitAt 2 (tail ls)

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