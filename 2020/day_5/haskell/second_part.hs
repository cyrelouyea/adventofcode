module Main where

import qualified Data.IntSet as Set
import System.IO

bitSize = 10

main :: IO ()
main = do
  m <- doReadFile
  case findMySeat $ Set.fromList $ map decodeSeat m of
    Nothing -> error "Seat not found"
    Just s -> print s
  return ()

findMySeat :: Set.IntSet -> Maybe Set.Key
findMySeat s =
  findFirst (`Set.notMember` s) [Set.findMin s .. Set.findMax s]

findFirst :: (a -> Bool) -> [a] -> Maybe a
findFirst f l
  | null l = Nothing
  | f (head l) = Just (head l)
  | otherwise = findFirst f (tail l)

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
      (\i b -> b * 2 ^ (bitSize - i - 1))
      [0 ..]
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
