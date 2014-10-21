{-
  Написать программу, которая в зависимости от параметров командной строки
  а) генерирует случайный текстовый файл содержащий декартовы координаты точек на плоскости
     (по одной точке в каждой строке);
  б) определяет по заданному файлу в указанном ранее формате количество точек в каждой
     из четвертей;
  в) отыскивает наиболее удалённую от начала координат точку.
-}

import System.Environment
import System.Directory
import Data.List
import Data.Char
import System.IO
import System.Random

main = do
	[op, file] <- getArgs
	doOperations op file
	
doOperations "1" file = generatePointFile file
doOperations "2" file = getCountPointsInQuarters file
--doOperations "2" file = getFarPoint file

generatePointFile file = do
	gen <- getStdGen
	let [count, val] = take 2 $ randomRs (1, 3000) gen :: [Int]
	writeFile file $ unlines $ map show $ take count $ zipWith (\x y -> (x, y)) (randomRs ((-val), val) (mkStdGen 23435) :: [Int]) (randomRs ((-val), val) (mkStdGen 9813) :: [Int])
	
getCountPointsInQuarters file = do
	contents <- readFile file
	let counts = foldl (\acc x -> countPointsInQuarters acc (read x)) (0, 0, 0, 0) (lines contents)
	putStrLn $ show counts
	
countPointsInQuarters (c1, c2, c3, c4) (x, y)
	| x >= 0 && y >= 0 = (c1 + 1, c2, c3, c4)
	| x < 0 && y >= 0 = (c1, c2 + 1, c3, c4)
	| x < 0 && y < 0 = (c1, c2, c3 + 1, c4)
	| otherwise = (c1, c2, c3, c4 + 1)
	
checkDistant (x1, y1) (x2, y2) = ((x1^2) + (y1 ^2)) < ((x2^2) + (y2^2))

getFarPoint file = do
	contents <- readFile file
	let farPoint = foldl (\acc x -> checkDistant acc (read x)) (0, 0) (lines contents)
	putStrLn $ show farPoint
