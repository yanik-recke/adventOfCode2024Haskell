import System.IO
import Control.Monad

main = do  
        contents <- readFile "input01.txt"
        print $ calcSol (cl even (map read . words $ contents)) (cl odd (map read . words $ contents))
            where 
                cl :: (Integer -> Bool) -> [Integer] -> [Integer]
                cl f = map snd . filter (\ (i, _) -> f i ) . zip [1..]

-- part 1
calcSol :: [Integer] -> [Integer] -> Integer
calcSol [x] [y] = abs (x - y)
calcSol (x : xs) (y : ys)   = 
    let minX = foldr min x xs; minY = foldr min y ys in
    abs (minX - minY) + calcSol (remove (x : xs) minX) (remove (y : ys) minY)
    where
        remove :: [Integer] -> Integer -> [Integer]
        remove [] _         = []
        remove (x : xs) y   | x == y = xs
                            | otherwise = x : remove xs y

-- part 2
calcSol' :: [Int] -> [Int] -> Int
calcSol' xs ys = foldr (\ x y -> y + (x  * (length . filter (==y)) ys)) 0 xs
