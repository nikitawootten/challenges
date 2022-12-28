import System.IO (withFile, hGetContents, IOMode(ReadMode))
import Data.List.Split (splitOn)
import Text.Printf (printf)
import Data.Set (Set, fromList, toList)

allDifferent :: [Char] -> Bool
allDifferent input = length input == length (toList $ fromList input)

-- |
-- Finds the end index where the previous `windowSize` characters are all different (1-indexed)
findAllDifferent :: Int -> [Char] -> Int -> Int
findAllDifferent windowSize tokens i = do
    let head = take windowSize tokens
    let tail = drop windowSize tokens
    if allDifferent head then i + windowSize else findAllDifferent windowSize (drop 1 tokens) (i + 1)

-- |
-- >>> part1 "bvwbjplbgvbhsrlpgdmjqwftvncz"
-- 5
-- >>> part1 "nppdvjthqldpwncqszvftbrmjlhg"
-- 6
-- >>> part1 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
-- 10
-- >>> part1 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
-- 11
part1 :: [Char] -> Int
part1 input = findAllDifferent 4 input 0

-- |
-- >>> part2 "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
-- 19
-- >>> part2 "bvwbjplbgvbhsrlpgdmjqwftvncz"
-- 23
-- >>> part2 "nppdvjthqldpwncqszvftbrmjlhg"
-- 23
-- >>> part2 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
-- 29
-- >>> part2 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
-- 26
part2 :: [Char] -> Int
part2 input = findAllDifferent 14 input 0

main :: IO ()
main = withFile "input.txt" ReadMode $ \handle -> do
    contents <- hGetContents handle
    printf "Part 1: %v\n" $ part1 contents
    printf "Part 2: %v\n" $ part2 contents

