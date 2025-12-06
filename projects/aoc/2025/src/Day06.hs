module Day06 (part1, part2) where

import Data.List (maximumBy, transpose)
import Data.Ord (comparing)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec, eof, oneOf, parse, sepEndBy1)
import Text.Megaparsec.Char (eol, hspace, hspace1, space)
import Text.Megaparsec.Char.Lexer as L (decimal)
import Text.Printf (printf)

type Parser = Parsec Void String

part1 :: String -> Either (ParseErrorBundle String Void) Int
part1 = Text.Megaparsec.parse parser ""
  where
    parser :: Parser Int
    parser = do
      numbers <- sepEndBy1 (hspace *> sepEndBy1 L.decimal hspace1) eol
      _ <- hspace
      operations <- sepEndBy1 (oneOf "+*") hspace
      _ <- space
      _ <- eof
      return $ sum $ zipWith ($) (getOperation <$> operations) (transpose numbers)
      where
        getOperation c
          | c == '+' = sum
          | otherwise = product

part2 :: String -> Either (ParseErrorBundle String Void) Int
part2 = part1 -- TODO, broken below
-- part2 = Text.Megaparsec.parse parser ""
--   where
--     parser :: Parser Int
--     parser = do
--       numbers <- sepEndBy1 (hspace *> sepEndBy1 L.decimal hspace1) eol
--       _ <- hspace
--       operations <- sepEndBy1 (oneOf "+*") hspace
--       _ <- space
--       _ <- eof
--       let
--         getOperation c
--           | c == '+' = sum
--           | otherwise = product
--         ops = getOperation <$> operations
--         cols = translateWritingSystem <$> transpose numbers
--         translateWritingSystem nums = read <$> transpose (reverse . show <$> nums)
--         maxDigits :: [Int] -> Int
--         maxDigits nums = maximum $ length . show <$> nums
--         padNumbers :: Int -> Int -> String
--         padNumbers padding = printf $ "%0" <> show padding <> "d"
--       return $ sum $ zipWith ($) ops cols
