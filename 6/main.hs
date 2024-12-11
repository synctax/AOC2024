import Control.Arrow ((&&&))
import Data.List


type Input = [String]
type Coord = (Int, Int)

infix 4 !!!
(!!!) :: [[a]] -> (Int, Int) -> a
l !!! (x,y) = l !! x !! y

both :: (a->b) -> (a,a) -> (b,b)
both f (a,b) = (f a, f b)

rlA :: [[a]] -> [[a]]
rlA = reverse . transpose

-- assumes square
rlC :: Coord -> Int ->  Coord
rlC (a,b) s = (b,s-a)

rrC :: Coord -> Int -> Coord
rrC (a,b) s = (s-b, a)

iterateN :: Int -> (a->a) -> a -> a
iterateN n f x = last $ take (n+1) $ iterate f x 

-- assumes square
inBounds :: [[a]] -> Coord -> Bool
inBounds  a = uncurry (&&) . both (>= length a)

findStart :: [String] -> Coord
findStart =  fst . head . filter (isLilGuy . snd) . assocs
    where
        assocs a = [((x,y),v) | (y,as) <- zip [0..] a, (x,v) <- zip [0..] as]
        isLilGuy = flip elem "<>^v"

part1 :: Input -> [Coord]
part1 m = (traversePath 0 <*> (findStart)) $ transpose $ reverse m
    where
traversePath :: Int -> [String] -> Coord -> [Coord]
traversePath t m (x,y) = case nextObstacle m of
    Just i -> [iterateN t (flip rrC l) (y,nx) | nx <- [x..(i+x-1)]] <> traversePath newT (rlA m) (rlC (i-1, y) l)
    Nothing -> []
    where
        nextObstacle = elemIndex '#' . drop x . (!! y)
        newT = mod (t+1) 4
        l = (length m)-1

part2 :: Input -> Int
part2 = const 0

prepare :: String -> Input
prepare = lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare