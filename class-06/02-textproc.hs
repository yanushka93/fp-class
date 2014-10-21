{-
  Разработайте утилиту со следующими возможностями:
  1) подсчёт количества строк в заданном текстовом файле;
  2) добавление заданной строки в начало (конец) заданного файла;
  3) преобразование всех буквенных символов заданного файла к верхнему
     регистру (результат выводится на консоль);
  4) построчное слияние двух заданных файлов (каждая строка первого файла
     соединяется с соответствующей строкой второго файла);
  5) генерация случайного текстового файла (случайность должна ограничиваться
     максимальным количеством строк в файле и символов в строке).

  Все входные данные программы должны передаваться с помощью параметров
  командной строки.
-}


import System.Environment
import System.Directory
import Data.List
import Data.Char
import System.IO
import System.Random

main = do
	(op : filename : args) <- getArgs
	doOperations (read op) filename args

	
doOperations 1 filename _ = getCountLines filename
doOperations 2 filename args = addNewLine filename args
doOperations 3 filename _ = textToUpper filename
doOperations 4 filename args = fileMerge filename args
doOperations 5 filename args = withFile filename WriteMode (\handle -> do
	getRandomFile handle (map read args))
doOperations _ _ _ = undefined
	
getCountLines filename = do
	contents <- readFile filename
	putStrLn $ show $ length $ lines contents
	
addNewLine filename (str : "begin" : _) = do
	contents <- readFile filename
	writeFile filename (str ++ "\n" ++ contents)
	
addNewLine filename (str : "end" : _) = do
	appendFile filename ("\n" ++ str)
	
textToUpper filename = do
	contents <- readFile filename
	putStrLn $ map toUpper contents
	
fileMerge file1 (file2 : _ ) = do
	contents1 <- readFile file1
	contents2 <- readFile file2
	writeFile (file1 ++ file2) $ unlines $ zipWith (++) (lines contents1) (lines contents2)
	
getRandomFile handle (maxstr : maxlen : _) =
	if maxstr > 0
	then do
		rndstr <- getRandomStr maxlen
		hPutStrLn handle rndstr
		getRandomFile handle [maxstr - 1, maxlen]
	else
		return ()
		
getRandomStr maxlen = do
	gen <- getStdGen
	return $ take maxlen $ randomRs ('a','z') gen