import Control.Arrow ((&&&))
import Data.List.Split
import GHC.Base (VecElem(Int16ElemRep))
import Data.List (sortBy)

type Rule = (Int, Int)
type Update = [Int]
type Input = ([Rule], [Update])

isValid :: [Rule] -> [Int] -> Bool
isValid rs [] = True
isValid rs (u:us) | followsRules = isValid rs us
                  | otherwise = False
    where
        followsRules = (==0) $ length $ filter (flip elem us) $ priorValues u rs
        priorValues v = map fst . filter ((==v) . snd)
takeMiddle :: [Int] -> Int
takeMiddle =  head . (flip drop <*> ((flip div 2) . length))

part1 :: Input -> Int
part1 (r,u) =  sum $ map takeMiddle $ filter (isValid r) u

part2 :: Input -> Int
part2 (r,u) = sum $ map takeMiddle $ map reorder validu
    where
        validu = filter (not . isValid r) u
        reorder u = sortBy (ruleCompare) u
        ruleCompare :: Int -> Int -> Ordering
        ruleCompare a b | findRule a b == (a,b) = LT
                        | findRule a b == (b,a) = GT
                        | otherwise = EQ
            where
                findRule a b = case  filter (liftA2 (||) (==(a,b)) (==(b,a))) r of 
                    [] -> (0,0)
                    a -> head a

prepare :: String -> Input
prepare =  (rules . head &&& updates . last) . splitOn [""] . lines
    where
        rules = map (liftA2 (,) (head) (last) . map read . splitOn "|")
        updates = map (map read . splitOn ",")


main :: IO()
main = readFile "input.txt" >>= print  . (part1 &&& part2) . prepare