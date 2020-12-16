module Main where

import System.IO
import qualified Data.Map as Map

type Passport = Map.Map String String
type PassportRule = ([Char], [Char] -> Bool)

main :: IO ()
main = do
  m <- doReadFile
  let passports = parsePassports m
  print $ length $ filter isValid passports
  return ()

requiredFields :: [PassportRule]
requiredFields = 
  [ ("byr", const True)
  , ("iyr", const True)
  , ("eyr", const True)
  , ("hgt", const True)
  , ("hcl", const True)
  , ("ecl", const True)
  , ("pid", const True)
  ]

isValid :: Passport -> Bool 
isValid p = isValid' p requiredFields

isValid' :: Passport -> [PassportRule] -> Bool 
isValid' p rf = 
  null rf ||
  case mvalue of
    Nothing -> False
    Just value -> check value && isValid' p (tail rf)
  where mvalue = Map.lookup field p
        field = (fst.head) rf
        check = (snd.head) rf

parsePassports :: [[Char]] -> [Passport]
parsePassports ps
  | null ps = [] 
  | otherwise = parsePassport ((words . unwords) passport) : parsePassports (tail rest)
      where (passport, rest) = break null ps

parsePassport :: [[Char]] -> Passport
parsePassport p 
  | null p = Map.empty
  | otherwise = Map.union (Map.fromList [(field, tail value)]) (parsePassport (tail p))
      where (field, value) = break (==':') (head p)

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
