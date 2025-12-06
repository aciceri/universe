import Day01 qualified
import Day02 qualified
import Day03 qualified
import Day04 qualified
import Day05 qualified
import Day06 qualified
import Test.Hspec
import Text.Printf (printf)

daySpec ::
  (Show e1, Eq e1, Show e2, Eq e2) =>
  Int ->
  (String -> Either e1 Int) ->
  Int ->
  (String -> Either e2 Int) ->
  Int ->
  Spec
daySpec n p1 e1 p2 e2 = describe ("Day" ++ show n) $ do
  let file = printf "inputs/day%02d.txt" n
  it "part1" $ do
    input <- readFile file
    p1 input `shouldBe` Right e1
  it "part2" $ do
    input <- readFile file
    p2 input `shouldBe` Right e2

main :: IO ()
main = hspec $ do
  daySpec 1 Day01.part1 1147 Day01.part2 6789
  daySpec 2 Day02.part1 54641809925 Day02.part2 73694270688
  daySpec 3 Day03.part1 16887 Day03.part1 16887 -- TODO part 2
  daySpec 4 Day04.part1 1435 Day04.part2 8623
  daySpec 5 Day05.part1 661 Day05.part2 359526404143208
  daySpec 6 Day06.part1 3261038365331 Day06.part1 3261038365331
