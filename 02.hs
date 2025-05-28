import System.IO
import Control.Monad
import Data.Text (splitOn)

p1 = do  
        contents <- readFile "input02.txt"
        print $ solve' . (map (map readInt . words) . lines) $ contents


readInt :: [Char] -> Integer
readInt = read

-- part 1
solve' :: [[Integer]] -> Int
solve' = length . filter (\ x -> diff x && (order x || reverseOrder x))

diff :: [Integer] -> Bool
diff (x : xs) = diff' x xs
    where
        diff' :: Integer -> [Integer] -> Bool
        diff' p []            = True
        diff' p (y : ys)      = let d = abs $ y - p in ((d < 4 && d > 0) && diff' y ys)

order :: [Integer] -> Bool
order (x : xs) = foldr (\(l, r) y -> y && (l <= r)) True $ zip (x : xs) xs

reverseOrder :: [Integer] -> Bool
reverseOrder (x : xs) = foldr (\(l, r) y -> y && (l >= r)) True $ zip (x : xs) xs


-- part 2
p2 = do  
        contents <- readFile "input02.txt"
        print $ solve'' . (map (map readInt . words) . lines) $ contents

solve'' :: [[Integer]] -> Int
solve'' = length . filter (atLeastOne . removeOne)

atLeastOne :: [[Integer]] -> Bool
atLeastOne = foldr (\ x y -> y || (diff x && (order x || reverseOrder x))) False

removeOne :: [Integer] -> [[Integer]]
removeOne xs = xs : [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]]