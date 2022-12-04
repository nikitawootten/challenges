import System.IO (withFile, hGetContents, IOMode(ReadMode))
import Data.List.Split (splitOn)
import Data.Char (isSpace)
import Text.Printf (printf)

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

data Move = Rock | Paper | Scissors
    deriving (Eq)

decodeAdversary :: String -> Move
decodeAdversary "A" = Rock
decodeAdversary "B" = Paper
decodeAdversary "C" = Scissors

decodeLine :: [String] -> (Move, String)
decodeLine (adversary:self:tail) = (decodeAdversary adversary, self)

cleanInput :: String -> [(Move, String)]
cleanInput input = do
    let clean = trim input
    let lines = splitOn "\n" clean
    let cleanLines = map (splitOn " ") lines
    map decodeLine cleanLines

winningMove :: Move -> Move
winningMove Rock = Paper
winningMove Paper = Scissors
winningMove Scissors = Rock

outcomeScore :: Move -> Move -> Int
outcomeScore adversary self
    | self == winningMove adversary = 6
    | self == adversary = 3
    | otherwise = 0

moveScore :: Move -> Int
moveScore Rock = 1
moveScore Paper = 2
moveScore Scissors = 3

type Decoder = (Move -> String -> Move) -- Decoders read the second column (X,Y,Z) and produce the appropriate move

playScore :: Decoder -> (Move, String) -> Int
playScore decoder (adversary, encodedSelf) = do
    let self = decoder adversary encodedSelf
    outcomeScore adversary self + moveScore self

totalScore :: [(Move, String)] -> Decoder -> Int
totalScore plays decoder = sum $ map (playScore decoder) plays

decodeSelfMove :: Decoder -- The naive encoding, X Y and Z correspond to moves
decodeSelfMove adversary "X" = Rock
decodeSelfMove adversary "Y" = Paper
decodeSelfMove adversary "Z" = Scissors

part1 plays = totalScore plays decodeSelfMove

losingMove :: Move -> Move
losingMove Rock = Scissors
losingMove Paper = Rock
losingMove Scissors = Paper

decodeSelfOutcome :: Decoder -- The actual encoding, X Y and Z correspond to outcomes
decodeSelfOutcome adversary "X" = losingMove adversary
decodeSelfOutcome adversary "Y" = adversary -- draw always plays adversary's move
decodeSelfOutcome adversary "Z" = winningMove adversary

part2 plays = totalScore plays decodeSelfOutcome 

main :: IO ()
main = withFile "input.txt" ReadMode $ \handle -> do
    contents <- hGetContents handle
    let input = cleanInput contents
    printf "Part 1: %v\n" (part1 input)
    printf "Part 2: %v\n" (part2 input)