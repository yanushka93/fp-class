{-
  Создать текстовый файл, содержащий случайные целые числа, разделённые пробелами
  и символами перевода строки. В командной строке должны задаваться следующие параметры:
  1) имя создаваемого файла;
  2) диапазон генерируемых случайных чисел: от и до;
  3) количество чисел в строке;
  4) количество строк в файле.
-}

import System.Environment
import System.IO
import System.Directory
import System.Random
import Control.Monad

generateFile filename bounds count rows = do
	contents <- generateContent bounds count rows
	writeFile filename contents

generateContent bounds count rows = do
	content <- liftM unlines $ replicateM rows $ generateStr bounds count 
	return content

generateStr bounds count = do
	gen <- newStdGen
	return $ unwords $ map show $ take count (randomRs bounds gen :: [Int])
	
main = do
	[filename, min, max, count, rows] <- getArgs
	generateFile filename (read min :: Int, read max :: Int) (read count :: Int) (read rows :: Int)
