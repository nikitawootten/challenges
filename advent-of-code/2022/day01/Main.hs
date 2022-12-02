import System.IO (withFile, hGetContents, IOMode(ReadMode))
import Data.List.Split (splitOn)
import Data.List (sortBy, sortOn)
import Data.Char (isSpace)
import Text.Printf (printf)
import Data.Ord (Down(Down))

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

toInt :: [Char] -> Int
toInt x = read x :: Int

cleanInput :: String -> [[Int]]
cleanInput input = do
  let cleanInput = trim input
  let lines = splitOn "\n\n" cleanInput
  let chunks = map (splitOn "\n") lines
  map (map toInt) chunks

sumList :: [Int] -> Int
sumList (head:tail) = foldr (+) head tail

day1 = maximum

sortDesc :: [Int] -> [Int]
sortDesc = sortOn Down

day2 x = sumList $ take 3 $ sortDesc x

main :: IO ()
main = withFile "input.txt" ReadMode $ \handle -> do
  contents <- hGetContents handle
  let itemsPerElf = cleanInput contents
  let totalCaloriesPerElf = map sumList itemsPerElf
  printf "Day 1: %v\n" (day1 totalCaloriesPerElf)
  printf "Day 2: %v\n" (day2 totalCaloriesPerElf)