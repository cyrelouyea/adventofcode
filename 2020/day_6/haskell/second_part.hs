module Main where

import Data.List
import qualified Data.Set as Set
import System.IO

type Form = Set.Set Char

type Group = [Form]

main :: IO ()
main = do
  m <- doReadFile
  print $ sum $ map (Set.size . intersections) (parseGroups m)
  return ()

intersections :: Ord a => [Set.Set a] -> Set.Set a
intersections ss = foldl Set.intersection (head ss) ss

parseGroups :: [[Char]] -> [Group]
parseGroups fs
  | null fs = []
  | null rest = [map Set.fromList group]
  | otherwise = map Set.fromList group : parseGroups (tail rest)
  where
    (group, rest) = break null fs

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
