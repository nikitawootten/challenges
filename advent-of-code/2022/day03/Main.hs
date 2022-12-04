import System.IO (withFile, hGetContents, IOMode(ReadMode))
import Data.Char (isSpace, ord)
import Data.List.Split (splitOn)
import Data.Set (Set, fromList, intersection, union, toList)
import Text.Printf (printf)

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

data Rucksack = Rucksack {
    compartment1 :: Set Char,
    compartment2 :: Set Char
} deriving Show

fromLine :: [Char] -> Rucksack
fromLine input = do
    let (first, second) = splitAt ((length input + 1) `div` 2) input
    Rucksack (fromList first) (fromList second)

parseInput :: [Char] -> [Rucksack]
parseInput input = do
    let cleanInput = trim input
    map fromLine $ splitOn "\n" cleanInput

rucksackCommonItem :: Rucksack -> Char
rucksackCommonItem sack = head $ toList $ intersection (compartment1 sack) (compartment2 sack)

-- todo
itemScore :: Char -> Int
itemScore item
    | item >= 'a' = ord item - 96
    | otherwise = ord item - 64 + 26

part1 :: [Rucksack] -> Int
part1 sacks = do
    let commonItems = map rucksackCommonItem sacks
    sum $ map  itemScore commonItems

sackUnion :: Rucksack -> Set Char
sackUnion sack = compartment1 sack `union` compartment2 sack

intersection3 first second third = intersection first (intersection second third)

groupBadges :: [Rucksack] -> [Char]
groupBadges [] = []
groupBadges (first:second:third:tail) = do
    let groupBadge = head $ toList $ intersection3 (sackUnion first) (sackUnion second) (sackUnion third)
    groupBadge : groupBadges tail

part2 sacks = do
    let badges = groupBadges sacks
    sum $ map itemScore badges

main :: IO ()
main = withFile "input.txt" ReadMode $ \handle -> do
    contents <- hGetContents handle
    let sacks = parseInput contents
    printf "Part 1: %v\n" $ part1 sacks
    printf "Part 2: %v\n" $ part2 sacks