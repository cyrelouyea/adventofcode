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
  print $ nbBagsWithBag "shiny gold" (parseBags m)
  return ()

nbBagsWithBag :: String -> Bags -> Int
nbBagsWithBag bagToFind bags =
  length $
    filter
      (\bagName -> bagContains bagName bagToFind bags)
      (Map.keys bags)

bagContains :: String -> String -> Bags -> Bool
bagContains bagName bagToFind bags
  | Map.null bagContents = False
  | Map.member bagToFind bagContents = True
  | otherwise = any (\b -> bagContains b bagToFind bags) (Map.keys bagContents)
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
