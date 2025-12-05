module Day02 (part1, part2) where

import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec, parse, sepBy)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L
import Prelude hiding (max, min)

type Parser = Parsec Void String

data Direction = L | R deriving (Show)

data Range = Range Int Int deriving (Show)

range :: Parser Range
range = do
  min <- L.decimal :: Parser Int
  _ <- char '-' :: Parser Char
  max <- L.decimal :: Parser Int
  return $ Range min max

parseRanges :: String -> Either (ParseErrorBundle String Void) [Range]
parseRanges = parse parser ""
  where
    parser :: Parser [Range]
    parser = range `sepBy` char ','

numDigits :: Int -> Int
numDigits n = floor $ logBase (10 :: Double) (fromIntegral n) + 1

isRepetitive :: Int -> Bool
isRepetitive n
  | odd digits = False
  | otherwise = n `mod` ((10 ^ (digits `div` 2)) + 1) == 0
  where
    digits = numDigits n

sumWrongIds' :: Range -> Int
sumWrongIds' (Range min max) = sum $ filter isRepetitive [min .. max]

sumWrongIds :: [Range] -> Int
sumWrongIds ranges = sum $ sumWrongIds' <$> ranges

part1 :: String -> Either (ParseErrorBundle String Void) Int
part1 input = sumWrongIds <$> parseRanges input

getDivisors :: Int -> [Int]
getDivisors n = [m | m <- [2 .. n], n `mod` m == 0]

repeatNum :: Int -> Int -> Int
repeatNum n times = read $ concat $ replicate times $ show n

firstDigits :: Int -> Int -> Int
firstDigits n num = read $ take n $ show num

isRepetitivePart2 :: Int -> Bool
isRepetitivePart2 n = any (\d -> repeatNum (firstDigits (digits `div` d) n) d == n) $ getDivisors digits
  where
    digits = numDigits n

sumWrongIdsPart2' :: Range -> Int
sumWrongIdsPart2' (Range min max) = sum $ filter isRepetitivePart2 [min .. max]

sumWrongIdsPart2 :: [Range] -> Int
sumWrongIdsPart2 ranges = sum $ sumWrongIdsPart2' <$> ranges

part2 :: String -> Either (ParseErrorBundle String Void) Int
part2 input = sumWrongIdsPart2 <$> parseRanges input
