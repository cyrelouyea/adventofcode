module Main where

import Data.Array
import qualified Data.Either as Either
import qualified Data.IntSet as IntSet
import System.IO

type Instruction = (String, Int)

type Program = Array Int Instruction

main :: IO ()
main = do
  m <- doReadFile
  case findFirst
    (\p -> Either.isRight $ runProgram p 0 0 IntSet.empty)
    (allPossiblePrograms (parseProgram m) 0) of
    Nothing -> error "No program found"
    Just p -> print $ runProgram p 0 0 IntSet.empty
  return ()

allPossiblePrograms :: Program -> Int -> [Program]
allPossiblePrograms p i
  | i > snd (bounds p) = []
  | op == "jmp" || op == "nop" = [p, newP] ++ allPossiblePrograms p (i + 1)
  | otherwise = allPossiblePrograms p (i + 1)
  where
    (op, val) = p ! i
    newP = p // [(n, (changeOp op, val)) | n <- [i]]

runProgram :: Program -> Int -> Int -> IntSet.IntSet -> Either (Int, Int) Int
runProgram p i acc visited
  | isFinished = Right acc
  | IntSet.member i visited = Left (i, acc)
  | otherwise = runProgram p newI newAcc newVisited
  where
    isFinished = i > snd (bounds p)
    newVisited = IntSet.union (IntSet.singleton i) visited
    (newI, newAcc) = evalInstruction i acc (p ! i)

changeOp :: String -> String
changeOp op
  | op == "jmp" = "nop"
  | op == "nop" = "jmp"
  | otherwise = error $ "Wrong operation: " ++ show op

evalInstruction :: Int -> Int -> Instruction -> (Int, Int)
evalInstruction i acc instruction
  | op == "nop" = (i + 1, acc)
  | op == "jmp" = (i + val, acc)
  | op == "acc" = (i + 1, acc + val)
  | otherwise = error $ "Unknown operator: " ++ show op
  where
    (op, val) = instruction

parseProgram :: [String] -> Program
parseProgram is =
  listArray (0, length instructions - 1) instructions
  where
    instructions = parseInstructions is

parseInstructions :: [String] -> [Instruction]
parseInstructions is
  | null is = []
  | otherwise = parseInstruction (head is) : parseInstructions (tail is)

parseInstruction :: String -> Instruction
parseInstruction i = (op, val)
  where
    (op, s) = break (== ' ') i
    sigval = tail s
    val = m * read (tail sigval)
    m =
      if head sigval == '+'
        then 1
        else -1

findFirst :: (a -> Bool) -> [a] -> Maybe a
findFirst f l
  | null l = Nothing
  | f (head l) = Just (head l)
  | otherwise = findFirst f (tail l)

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
