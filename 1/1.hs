import Data.List

splitcast :: String -> [Int]
splitcast line = map read (words line)

distance :: Num a => a -> a -> a
distance a b = abs $ a - b 

main = do
    let file = "input-1.txt"
    contents <- readFile file
    let nested = map splitcast (lines contents)
    let leftSide = map head nested
    let rightSide = map last nested
    let diff = zipWith distance (sort leftSide) (sort rightSide)
    print (sum diff)
    