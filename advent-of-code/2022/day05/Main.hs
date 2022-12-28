import System.IO (withFile, hGetContents, IOMode(ReadMode))
import Data.List.Split (splitOn)
import Data.List (splitAt)
import Text.Printf (printf)

toInt :: [Char] -> Int
toInt x = read x :: Int

type Stacks = [[Char]]

-- initialize the empty stacks structure
emptyStacks :: Int -> Stacks
emptyStacks 0 = [[]]
emptyStacks size = []:emptyStacks (size - 1)

stacksInsert :: Stacks -> Char -> Int -> Stacks
stacksInsert stacks item col = do
    let stackHead = take col stacks
    let (stackTarget:stackTail) = drop col stacks
    stackHead ++ ((item:stackTarget):stackTail)

parseStacksRows :: Stacks -> [[Char]] -> Int -> Stacks
parseStacksRows stacks [] _ = stacks -- base case, no remaining rows
parseStacksRows stacks ([]:remainingRows) _ = parseStacksRows stacks remainingRows 0
parseStacksRows stacks ((_:item:_:tail):remainingRows) i = do
    let remaining = drop 1 tail:remainingRows -- drop space between columns
    if item == ' ' -- if row is empty
        then parseStacksRows stacks remaining (i + 1) -- nothing has changed, move on
    else parseStacksRows (stacksInsert stacks item i) remaining (i + 1)

parseStacks :: [Char] -> Stacks
parseStacks input = do
    let lines = drop 1 $ reverse $ splitOn "\n" input
    let numStacks = length (head lines) `div` 4 -- determine size of stacks from line length
    parseStacksRows (emptyStacks numStacks) lines 0

type Command = (Int, Int, Int) -- Amount, From Index, To Index

parseCommand :: [Char] -> Command
parseCommand input = do
    let (_:amount:_:from:_:to:_) = splitOn " " input
    (toInt amount, toInt from, toInt to)

parseCommands :: [Char] -> [Command]
parseCommands input = map parseCommand $ splitOn "\n" input

parseInput :: [Char] -> (Stacks, [Command])
parseInput input = do
    let (rawStacks:rawCommands:_) = splitOn "\n\n" input
    (parseStacks rawStacks, parseCommands rawCommands)

-- Helper replace item in list
replaceAt n item list = before ++ (item:after) where (before, _:after) = splitAt n list

applyCommand :: Bool -> Stacks -> Command -> Stacks
applyCommand doReverse stacks (amount, from, to) = do
    let contents = if doReverse
        then reverse (take amount (stacks !! (from - 1)))
        else take amount (stacks !! (from - 1))
    let taken = replaceAt (from - 1) (drop amount (stacks !! (from - 1))) stacks
    replaceAt (to - 1) (contents++(taken !! (to - 1))) taken

tops = map head

part1 :: (Stacks, [Command]) -> [Char]
part1 (stacks, commands) = tops $ foldl (applyCommand True) stacks commands

part2 (stacks, commands) = tops $ foldl (applyCommand False) stacks commands


main :: IO ()
main = withFile "input.txt" ReadMode $ \handle -> do
  contents <- hGetContents handle
  let input = parseInput contents
  printf "Part 1: %v\n" $ part1 input
  printf "Part 2: %v\n" $ part2 input