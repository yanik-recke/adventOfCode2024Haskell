import System.IO
import Control.Monad
import Data.Text (splitOn)

main = do  
        contents <- readFile "input02.txt"
        print $ solve' . (map (map readInt . words) . lines) $ contents


readInt :: [Char] -> Integer
readInt = read

-- part 1
solve' :: [[Integer]] -> Int
solve' = length . filter (\ x -> diff x && (order x || reverseOrder x))
    where 
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
