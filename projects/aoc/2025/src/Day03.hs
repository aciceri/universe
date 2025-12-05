module Day03 (part1, part2) where

import Data.Char (digitToInt)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec, eof, parse, sepEndBy, some)
import Text.Megaparsec.Char (digitChar, eol)

type Parser = Parsec Void String

parseBanks :: String -> Either (ParseErrorBundle String Void) [[Int]]
parseBanks = parse parser ""
  where
    parser :: Parser [[Int]]
    parser = sepEndBy line eol <* eof
    line :: Parser [Int]
    line = some (digitToInt <$> digitChar)

firstDigit :: [Int] -> (Int, [Int])
firstDigit bank = (maxVal, digitsAfter)
  where
    maxVal = maximum (init bank) -- max value excluding the latest element
    maxPos = fromJust $ elemIndex maxVal bank
    digitsAfter = drop (maxPos + 1) bank

totalJoultage :: [[Int]] -> Int
totalJoultage = sum . fmap bankMaxJoultage
  where
    bankMaxJoultage bank = fDigit * 10 + secondDigit
      where
        (fDigit, otherDigits) = firstDigit bank
        secondDigit = maximum otherDigits

part1 :: String -> Either (ParseErrorBundle String Void) Int
part1 input = totalJoultage <$> parseBanks input

part2 :: String -> Either (ParseErrorBundle String Void) Int
part2 = part1 -- TODO
