module Day05 (part1, part2) where

import Data.Foldable (foldl')
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec, eof, many, parse, sepEndBy)
import Text.Megaparsec.Char (char, eol, newline)
import Text.Megaparsec.Char.Lexer as L (decimal)

type Parser = Parsec Void String

newtype Ingredient = Ingredient Int deriving (Show, Eq)

data Range = Range Int Int deriving (Show, Eq)

data Db = Db [Range] [Ingredient] deriving (Show, Eq)

parseDb :: String -> Either (ParseErrorBundle String Void) Db
parseDb = Text.Megaparsec.parse parser ""
  where
    parser :: Parser Db
    parser = do
      ranges <- parseRanges
      _ <- newline
      ingredients <- parseIngredients
      _ <- eof
      return $ Db ranges ingredients
    parseRanges :: Parser [Range]
    parseRanges = many $ do
      first <- L.decimal
      _ <- char '-'
      second <- L.decimal
      _ <- eol
      return $ Range first second
    parseIngredients :: Parser [Ingredient]
    parseIngredients = sepEndBy (Ingredient <$> L.decimal) eol

count :: (a -> Bool) -> [a] -> Int
count p = foldl' (\acc x -> acc + fromEnum (p x)) 0

countFreshIngredients :: Db -> Int
countFreshIngredients (Db ranges ingredients) = count isFresh ingredients
  where
    isFresh (Ingredient i) = any (\(Range a b) -> a <= i && i <= b) ranges

part1 :: String -> Either (ParseErrorBundle String Void) Int
part1 = fmap countFreshIngredients . parseDb

countUniqueFreshIngredients :: Db -> Int
countUniqueFreshIngredients (Db ranges _) = sum $ map (\(Range a b) -> b - a + 1) disconnectedRanges
  where
    disconnectedRanges = foldl' step [] ranges
    step previousRanges (Range inf sup)
      | null middleRanges = prevRanges <> [Range inf sup] <> nextRanges
      | otherwise = prevRanges <> [newMergedRange] <> nextRanges
      where
        (prevRanges, rest) = span (\(Range _ b) -> b < inf) previousRanges
        (middleRanges, nextRanges) = span (\(Range a _) -> a <= sup) rest
        minMiddleRanges = case head middleRanges of Range a _ -> a
        maxMiddleRanges = case last middleRanges of Range _ b -> b
        newMergedRange = Range (min minMiddleRanges inf) (max maxMiddleRanges sup)

part2 :: String -> Either (ParseErrorBundle String Void) Int
part2 = fmap countUniqueFreshIngredients . parseDb
