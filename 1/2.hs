import Data.List

splitcast :: String -> [Int]
splitcast line = map read (words line)

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

main = do
    let file = "input-1.txt"
    contents <- readFile file
    let nested = map splitcast (lines contents)
    let leftSide = map head nested
    let rightSide = map last nested
    let occurances = map (\x -> count x rightSide) leftSide
    let weighted = zipWith (*) leftSide occurances
    print (sum weighted)
    