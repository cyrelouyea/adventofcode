module Main where

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.IO

type BagContent = Map.Map String Int

type Bags = Map.Map String BagContent

main :: IO ()
main = do
  m <- doReadFile
  print $ nbBagsInside "shiny gold" (parseBags m)
  return ()

nbBagsInside :: String -> Bags -> Int
nbBagsInside bagName bags
  | Map.null bagContents = 0
  | otherwise =
    sum $
      map
        (\(bn, nb) -> nb * (1 + nbBagsInside bn bags))
        (Map.assocs bagContents)
  where
    bagContents = fromMaybe Map.empty (Map.lookup bagName bags)

parseBags :: [String] -> Bags
parseBags bs
  | null bs = Map.empty
  | otherwise =
    Map.union
      (Map.fromList [parseBag (head bs)])
      (parseBags (tail bs))

parseBag :: String -> (String, BagContent)
parseBag b = (bagName, bagContents)
  where
    bw = words b
    (bw1, rest) = break (== "contain") bw
    bagName = unwords $ takeWhile (/= "bags") bw1
    bagContents = parseBagContents (tail rest)

parseBagContents :: [String] -> BagContent
parseBagContents bc
  | null bc = Map.empty
  | rawNb == "no" = Map.empty
  | otherwise =
    Map.union
      (Map.singleton bagName (read rawNb))
      (parseBagContents (tail rest))
  where
    (bagContent, rest) = break (isPrefixOf "bag") bc
    rawNb = head bagContent
    bagName = unwords (tail bagContent)

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
