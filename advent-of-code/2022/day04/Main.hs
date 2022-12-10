import System.IO (withFile, hGetContents, IOMode(ReadMode))
import Data.Char (isSpace)
import Data.List.Split (splitOn)
import Text.Printf (printf)

toInt :: [Char] -> Int
toInt x = read x :: Int

type PairPair = ((Int, Int), (Int, Int))

fromPairs :: [[Char]] -> PairPair
fromPairs (elf1:elf2:_) = do
  let (elf1Lower:elf1Higher:_) = splitOn "-" elf1
  let (elf2Lower:elf2Higher:_) = splitOn "-" elf2
  ((toInt elf1Lower, toInt elf1Higher), (toInt elf2Lower, toInt elf2Higher))

fromLine :: [Char] -> PairPair
fromLine line = fromPairs $ splitOn "," line

parseInput :: [Char] -> [PairPair]
parseInput input = map fromLine $ splitOn "\n" input

boolToInt b = if b then 1 else 0

envelops :: PairPair -> Bool
envelops ((elf1Lower, elf1Higher), (elf2Lower, elf2Higher)) =
  (elf1Lower <= elf2Lower && elf2Higher <= elf1Higher) || (elf2Lower <= elf1Lower && elf1Higher <= elf2Higher)

decider pairCriteria input = do
  let decisions = map pairCriteria input
  sum $ map boolToInt decisions

part1 :: [PairPair] -> Int
part1 = decider envelops

overlaps :: PairPair -> Bool
overlaps ((elf1Lower, elf1Higher), (elf2Lower, elf2Higher)) =
  (elf1Lower <= elf2Lower && elf2Lower <= elf1Higher) ||
  (elf2Lower <= elf1Lower && elf1Lower <= elf2Higher)

part2 :: [PairPair] -> Int
part2 = decider overlaps

main :: IO ()
main = withFile "input.txt" ReadMode $ \handle -> do
  contents <- hGetContents handle
  let input = parseInput contents
  printf "Part 1: %v\n" $ part1 input
  printf "Part 2: %v\n" $ part2 input