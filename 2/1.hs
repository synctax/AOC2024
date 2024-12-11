import Control.Arrow ((&&&))
import Data.List


type Input = [[Int]]
data Sign = Neg | Zero | Pos
    deriving(Eq)

signOf :: Int -> Sign
signOf x | x == 0 = Zero
         | x < 0 = Neg
         | otherwise = Pos

distance :: Int -> Int -> Int
distance a = abs . subtract a  

filterCount :: (a->Bool) -> [a] -> Int
filterCount f = length . filter f 

-- part1 :: Input -> Int 
-- part1 = sum . map fromEnum . map (\(x:y:zs) -> isSafe (signOf $ y-x) (x:y:zs))
--     where 
--         isSafe _ [] = True
--         isSafe _ (x:[]) = True
--         isSafe s (x:y:zs) | valid = isSafe s (y:zs)
--                           | otherwise = False
--             where
--                 valid = d >= 1 && 
--                         d <= 3 && 
--                         signOf (y-x) == s
--                 d = distance x y

-- part2 :: Input -> Int
-- part2 = filterCount (<=1) . map (isSafe . diffs [])
--     where
--         diffs a [] = a
--         diffs a (x:[]) = a
--         diffs a (x:y:zs) = diffs (y-x:a) (y:zs)



--         intervalErrors = filterCount ((\x -> x>3 && x<1) . abs)

--         signErrors a = sum signCounts - maximum signCounts 
--             where
--                 signCounts = [ f a | f <- [countSign s| s <- [Zero, Neg, Pos]]]
--                 countSign s = filterCount (\x -> signOf x == s) 

-- part2 :: Input -> Int
-- part2 = sum . map fromEnum . map ( isValid . diffs [])
--     where
--         diffs a [] = a
--         diffs a (x:[]) = a
--         diffs a (x:y:zs) = diffs (y-x:a) (y:zs)

--         isValid a | ie == 0 && length g == 1 = True
--                   | length g > 2 || ie > 2 = False
--                   | (minimum $ map length g ) > 1 = False
--                   | otherwise = isValid (dampener [] g)
--                   where
--                     g = someGrouping a
--                     ie = intervalErrors a

--         dampener a [] = a
--         dampener a (x:xs) = case length x of
--             1 -> dampener a xs
--             otherwise -> dampener (sew a x) xs
--             where
--                 sew a (b:bs) = 

--         someGrouping = groupBy (\x -> \y ->signOf x == signOf y)
--         intervalErrors = filterCount ((\x -> x>3 || x<1) . abs)
--         signErrors a = sum signCounts - maximum signCounts 
--             where
--                 signCounts = [ f a | f <- [countSign s | s <- [Zero, Neg, Pos]]]
--                 countSign s = filterCount (\x -> signOf x == s) 

safe :: [Int] -> Bool
safe = liftA2 (&&) sameSign (all smallInterval) . diff
    where
        diff = zipWith (-) <*> tail
        smallInterval = liftA2 (&&) (>= 1) (<= 3) . abs
        sameSign (x:xs) = all (== signum x) $ map signum xs

part1 :: Input -> Int
part1 = length . filter safe

part2 :: Input -> Int
part2 = length . filter canBeMadeSafe
  where canBeMadeSafe = any safe . removeUpTo 1

removeUpTo :: Int -> [a] -> [[a]]
removeUpTo 0 xs = [xs]
removeUpTo _ [] = [[]]
removeUpTo n (x:xs) = ((x:) <$> removeUpTo n xs) <> removeUpTo (n - 1) xs



prepare :: String -> Input
prepare = map (map read . words) . lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare