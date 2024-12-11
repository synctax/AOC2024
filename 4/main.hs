import GHC.Arr
import Control.Arrow((&&&))

type Coord = (Int, Int)
type Grid = Array Coord Char

pointsDirs :: [Coord] -> [Coord] -> [(Coord, Coord)]
pointsDirs p d = [(a,b) | a <- p, b <- d]

onTuple :: (a->b->c) -> (a,a) -> (b,b) -> (c,c)
onTuple f a b= (f (fst a) (fst b), f (snd a) (snd b))

findWordWithDirections :: String -> [Coord] -> Grid -> [(Coord, Coord)]
findWordWithDirections s d g = filter (uncurry (grow s g)) $ pointsDirs (seeds (head s) g) d
    where
        seeds :: Char -> Grid -> [Coord]
        seeds c = map fst . filter ((== c ) . snd) . assocs 

        grow :: String -> Grid -> Coord -> Coord -> Bool
        grow "" _ _ _ = True
        grow (l:ls) g c d | not (inBounds c) = False
                        | g ! c == l = grow ls g n d
            where
                    n = onTuple (+) c d
                    inBounds = inRange (bounds g)
        grow _ _ _ _ = False

part1 :: Grid -> Int
part1 = length . findWordWithDirections "XMAS" directions
    where 
        directions :: [Coord]
        directions = [(x,y) | x <- [-1..1], y <- [-1..1]]
    
part2 :: Grid -> Int
part2 =  flip div 2 . length . (flip filter <*> isValid) . findWordWithDirections "MAS" directions
    where 
        directions = [(x,y) | x <- [-1,1], y <- [-1,1]]
        isValid :: [(Coord, Coord)] -> (Coord, Coord) -> Bool
        isValid all a = (>=2) $ length $ filter ((==(uncurry (onTuple (+)) a)) . uncurry (onTuple (+))) $ all

prepare :: String -> Grid
prepare =  parseArray . lines
    where
        parseArray rs = array ((1,1), (w,h)) [
            ((x, y), v) |
            (y, cs) <- zip [1..] rs,
            (x, v) <- zip [1..] cs
            ]
            where
                h = length rs
                w = case rs of
                    [] -> 0
                    cs : _ -> length cs 

main :: IO()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare