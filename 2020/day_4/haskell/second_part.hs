module Main where

import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.IO
import Text.Read

type Passport = Map.Map String String

type PassportRule = ([Char], [Char] -> Bool)

main :: IO ()
main = do
  m <- doReadFile
  let passports = parsePassports m
  print $ length $ filter (`isValid` requiredFields) passports
  return ()

requiredFields :: [PassportRule]
requiredFields =
  [ ("byr", byr),
    ("iyr", iyr),
    ("eyr", eyr),
    ("hgt", hgt),
    ("hcl", hcl),
    ("ecl", ecl),
    ("pid", pid)
  ]

hairColors :: Set.Set [Char]
hairColors = Set.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

isValid :: Passport -> [PassportRule] -> Bool
isValid p rf =
  null rf
    || case mvalue of
      Nothing -> False
      Just value -> check value && isValid p (tail rf)
  where
    mvalue = Map.lookup field p
    field = (fst . head) rf
    check = (snd . head) rf

byr :: [Char] -> Bool
byr v =
  case miv of
    Nothing -> False
    Just iv -> 1920 <= iv && iv <= 2002
  where
    miv = readMaybe v :: Maybe Int

iyr :: [Char] -> Bool
iyr v =
  case miv of
    Nothing -> False
    Just iv -> 2010 <= iv && iv <= 2020
  where
    miv = readMaybe v :: Maybe Int

eyr :: [Char] -> Bool
eyr v =
  case miv of
    Nothing -> False
    Just iv -> 2020 <= iv && iv <= 2030
  where
    miv = readMaybe v :: Maybe Int

hgt :: [Char] -> Bool
hgt v =
  case mnumber of
    Nothing -> False
    Just number -> case unit of
      "cm" -> 150 <= number && number <= 193
      "in" -> 59 <= number && number <= 76
      _ -> False
  where
    (strNumber, unit) = span isDigit v
    mnumber = readMaybe strNumber :: Maybe Int

hcl :: [Char] -> Bool
hcl v =
  hash == "#" && length strNumber == 6 && all isHexDigit strNumber
  where
    (hash, strNumber) = break isHexDigit v

ecl :: [Char] -> Bool
ecl v = Set.member v hairColors

pid :: [Char] -> Bool
pid v = length v == 9 && all isDigit v

parsePassports :: [[Char]] -> [Passport]
parsePassports ps
  | null ps = []
  | otherwise = parsePassport ((words . unwords) passport) : parsePassports (tail rest)
  where
    (passport, rest) = break null ps

parsePassport :: [[Char]] -> Passport
parsePassport p
  | null p = Map.empty
  | otherwise = Map.union (Map.fromList [(field, tail value)]) (parsePassport (tail p))
  where
    (field, value) = break (== ':') (head p)

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
