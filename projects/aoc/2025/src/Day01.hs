module Day01 (part1, part2) where

import Data.List (foldl')
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec, many, parse, (<|>))
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

data Direction = L | R deriving (Show)

data Instruction = Instruction Direction Int deriving (Show)

instruction :: Parser Instruction
instruction = do
  dir <- L <$ char 'L' <|> R <$ char 'R' :: Parser Direction
  num <- L.decimal :: Parser Int
  _ <- eol :: Parser [Char]
  return $ Instruction dir num

parseInstructions :: String -> Either (ParseErrorBundle String Void) [Instruction]
parseInstructions = parse parser ""
  where
    parser :: Parser [Instruction]
    parser = many instruction

countZeros :: [Instruction] -> Int
countZeros = fst . foldl' step (0, 50)
  where
    step :: (Int, Int) -> Instruction -> (Int, Int)
    step (zeros, pos) (Instruction dir n) = (zeros', newPos)
      where
        delta = case dir of
          L -> -n
          R -> n
        newPos = (pos + delta) `mod` 100
        zeros' = if newPos == 0 then zeros + 1 else zeros

countZeros' :: [Instruction] -> Int
countZeros' = fst . foldl' step (0, 50)
  where
    step :: (Int, Int) -> Instruction -> (Int, Int)
    step (zeros, pos) (Instruction dir n) = (zeros + crossed, newPos)
      where
        delta = case dir of
          L -> -n
          R -> n
        newPos = (pos + delta) `mod` 100
        crossed
          | delta >= 0 = abs $ (pos + delta) `div` 100
          | otherwise = ((pos + 99) `div` 100) - ((pos + delta + 99) `div` 100)

part1 :: String -> Either (ParseErrorBundle String Void) Int
part1 input = countZeros <$> parseInstructions input

part2 :: String -> Either (ParseErrorBundle String Void) Int
part2 input = countZeros' <$> parseInstructions input
