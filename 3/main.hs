import Control.Arrow
import Text.Regex.Posix
import Data.List
import Data.Function

data Toggle = Active | Deactive

mulMuls :: [String] -> [Int]
mulMuls = map (uncurry (*) . read . drop 3)

part1 :: String -> Int
part1 =  sum . mulMuls . getAllTextMatches . (=~ regex)
    where regex = "mul\\(([0-9]+,[0-9]+)\\)"

part2 :: String -> Int
part2 = sum . mulMuls . toggle Active . getAllTextMatches . (=~ regex)
    where 
        regex = "(mul\\(([0-9]+,[0-9]+)\\))|do\\(\\)|don't\\(\\)"

        toggle _ ("do()" : xs) = toggle Active xs
        toggle _ ("don't()" : xs) = toggle Deactive xs  
        toggle Active (x:xs) = x: toggle Active xs
        toggle Deactive (x:xs) = toggle Deactive xs
        toggle _ _ = []

main :: IO()
main = readFile "input.txt" >>= print . (part1 &&& part2)